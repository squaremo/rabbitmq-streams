/*
 * A sliding window with timeout and different ways to measure window length.
 */

package com.rabbitmq.streams.plugins

import scala.actors._
import Actor._
import Math._

import net.sf.json.JSONObject

import com.rabbitmq.streams.harness.{PipelineComponent, InputReader, InputMessage,
                                     NotificationType}

class WindowPlugin() extends PipelineComponent() {
  class ByteBasedBatcher(config:JSONObject) extends Actor{
    object Fullness extends Enumeration {val JUST_FULL, OVER_FULL, NOT_FULL = Value}

    val count = config.getInt("count")
    val overlap = config.getInt("overlap")
    val timeout = config.optLong("timeout", java.lang.Long.MAX_VALUE)
    val encoding: Seq[Char] = config.getString("encoding")
    val sep:String = encoding match {
      case Seq('u','t','f','-','8','-','s','e','p',':', rest@_*) => rest.mkString
    }
    val encodingOverhead = sep.getBytes().size

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
    System.err.println("###DEBUG constructed")
    def act() = {
      // System.err.println("###DEBUG acting")
      while (true) {
        receiveWithin(Math.max(0, deadline - System.currentTimeMillis)) {
          case msg: InputMessage => {
            System.err.println("###DEBUG receiving")
            msgs = msg::msgs
            howFullAfter(msg) match {
              case Fullness.JUST_FULL => ship(true)
              case Fullness.OVER_FULL => ship(false)
              case _ =>
                System.err.println("Not full yet: " + totalSize)
                ; //FIXME why doesn't scala give me a !@#$! exhaustiveness check?
            }
          }
          case TIMEOUT =>
            System.err.println("###DEBUG timeout: " + (deadline - System.currentTimeMillis))
            if (! msgs.isEmpty) { // FIXME to do otherwise seems impossible w/ current plugin API
              ship(true)
            }
        }
      }
    }
    def howFullAfter(msg:InputMessage) = {
      System.err.println("totalSize " + totalSize + " body.size " + msg.body.size +
                       " oh " + encodingOverhead + " msg " + new String(msg.body))
      (totalSize + msg.body.size + encodingOverhead) match {
        case size if size == count => Fullness.JUST_FULL
        case size if size > count  => Fullness.OVER_FULL
        case _ => Fullness.NOT_FULL
      }
    }
    def ship(justFull:Boolean) {
      System.err.println("About to ship " + justFull)
      val (msgsToSend:List[InputMessage], newMsgs:List[InputMessage], sizeDelta:Long) =
        if (justFull) {(msgs, List():List[InputMessage], 0L)}
        else          {(msgs.tail, List(msgs.head), msgs.head.body.size + encodingOverhead)}
      val concatedBodies = (msgsToSend.map((msg:InputMessage) => new String(msg.body)).
                            reverse.mkString(sep))
      publishToChannel("output", msgsToSend.head.withBody(concatedBodies))
      System.err.println("published " + concatedBodies)
      totalSize += sizeDelta
      msgs = newMsgs
//      state.put("msgs", msgs.map(_.toJson))
      setState(state)
    }
  }
  override def configure(config : JSONObject) {
    val batcher = config.getString("unit") match {
      case "B" | "bytes" =>  new ByteBasedBatcher(config)
    }
    batcher.start
    System.err.println("###DEBUG batcher created!")
    object input extends InputReader {
      override def handleMessage(msg : InputMessage) {
        System.err.println("###DEBUG about to batch")
        batcher !? msg // *synchronous* to retain transactionality
        System.err.println("###DEBUG batched")
      }
    }
    registerInput("input", input)
  }
}
