/*
 * A sliding window with timeout and different ways to measure window length.
 */

package com.rabbitmq.streams.plugins.window

import scala.actors._
import Actor._
import Math._

import net.sf.json.{JSONObject, JSONArray, JSONNull}

import com.rabbitmq.streams.harness.{PipelineComponent, InputReader, InputMessage,
                                     NotificationType, PluginException}

class WindowPlugin() extends PipelineComponent() {
  //FIXME(alexander) cut'n pasted hack
  def unpickleBody(body: Object): Array[Byte] = {
    JSONArray.toArray(JSONArray.fromObject(body)).asInstanceOf[Array[Object]].map(
      _.asInstanceOf[Int].toByte)
  }
  class ByteBasedBatcher(config:JSONObject) extends Actor{
    object Fullness extends Enumeration {
      val NOT_FULL, JUST_FULL, OVER_FULL, TWICE_FULL, IMPOSSIBLY_FULL = Value}

    val count = config.getInt("count")
    val overlap = config.getInt("overlap")
    val timeout =  if (! JSONNull.getInstance().equals(config.opt("timeout"))) (config.getDouble("timeout")*1000).asInstanceOf[Long]
                   else java.lang.Long.MAX_VALUE
    val encoding: Seq[Char] = config.getString("encoding")
    val sep:String = encoding match {
      case Seq('u','t','f','-','8','-','s','e','p',':', rest@_*) => rest.mkString
    }
    val encodingOverhead:Long = sep.getBytes().size

    var state: java.util.Map[java.lang.String,java.lang.Object] = getState()
    var (deadline:Long, msgs:List[InputMessage]) =
      if (! state.isEmpty) {
        (state.get("deadline").asInstanceOf[Long],
         List()) // FIXME
      }
      else {
        val dl = Math.max(timeout, timeout + System.currentTimeMillis)
        state.put("deadline", dl.asInstanceOf[Object])
        (dl, List())
      }

    // make totalSize for [] == -encodingOverhead to simplify things
    var totalSize:Long = (-encodingOverhead /: msgs){encodingOverhead+_+_.body.size}
    def act() {
      loop {
        receiveWithin(Math.max(0, deadline - System.currentTimeMillis)) {
          case msg: InputMessage => {
            msgs = msg::msgs
            howFullAfter(msg) match {
              case Fullness.JUST_FULL =>
                val (scala, sucks) = ship(true, msgs); msgs = scala; totalSize = sucks
              case Fullness.OVER_FULL =>
                val (scala, sucks) = ship(false, msgs); msgs = scala; totalSize = sucks
              case Fullness.TWICE_FULL =>
                ship(true, msgs.tail)
                val (scala, sucks) = ship(true, List(msgs.head)); msgs = scala; totalSize = sucks
              case Fullness.IMPOSSIBLY_FULL =>
                notifier.notify(NotificationType.BadData, "Message length(" + msg.body.size +
                                ") exceeds window count(" + count + ")")
                throw new PluginException("Illegal (too large) input msg for window plugin.")

              case _ =>
                totalSize += msg.body.size + encodingOverhead
//                System.err.println("Not full yet: " + totalSize)
                ; //FIXME why doesn't scala give me a !@#$! exhaustiveness check?
            }
//            System.err.println("###DEBUG about to reply msgs:" + msgs + " ")
            reply()
          }
          case TIMEOUT =>
           System.err.println("###DEBUG timeout: " + (deadline - System.currentTimeMillis))
            if (! msgs.isEmpty) { // FIXME to do otherwise seems impossible w/ current plugin API
              System.err.println("###DEBUG deadline ship")
              val (scala, sucks) = ship(true, msgs); msgs = scala; totalSize = sucks
              //FIXME(alexander) update state
            }
            deadline = Math.max(timeout, timeout + System.currentTimeMillis)
            state.put("deadline", deadline.asInstanceOf[Object])
            setState(state)
            reply()

         }

      }
    }
    def howFullAfter(msg:InputMessage) = {
//      System.err.println("totalSize " + totalSize + " body.size " + msg.body.size +
//                       " oh " + encodingOverhead + " msg " + msg.bodyAsString)
       val msgSize:Long = msg.body.size
      (totalSize + msgSize + encodingOverhead) match {
        case size if size > count && msgSize > count => Fullness.IMPOSSIBLY_FULL
        case size if size > count && msgSize == count => Fullness.TWICE_FULL
        case size if size > count => Fullness.OVER_FULL
        case size if size == count => Fullness.JUST_FULL
        case _ => Fullness.NOT_FULL
      }
    }
    def ship(withHead:Boolean, msgs:List[InputMessage]) = {
//      System.err.println("About to ship " + withHead)
      val (msgsToSend:List[InputMessage], newMsgs:List[InputMessage], newSize:Long) =
        if (withHead) (msgs, List():List[InputMessage], -encodingOverhead)
        else          (msgs.tail, List(msgs.head), msgs.head.body.size.asInstanceOf[Long])
      val concatedBodies = (msgsToSend.map((msg:InputMessage) => msg.bodyAsString).
                            reverse.mkString(sep))
      publishToChannel("output", msgsToSend.head.withBody(concatedBodies))
//      System.err.println("published " + concatedBodies)
//      state.put("msgs", msgs.map(_.toJson))
      setState(state)
//      System.err.println("state set " + state)
      (newMsgs, newSize)
    }
  }
  override def configure(config : JSONObject) {
    val batcher = config.getString("unit") match {
      case "B" | "bytes" =>  new ByteBasedBatcher(config)
    }
    batcher.start
    object input extends InputReader {
      override def handleMessage(msg : InputMessage) {
        batcher !? msg // *synchronous* to retain transactionality
      }
    }
    registerInput("input", input)
  }
}
