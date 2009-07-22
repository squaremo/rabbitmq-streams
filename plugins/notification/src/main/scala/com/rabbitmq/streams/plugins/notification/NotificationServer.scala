package com.rabbitmq.streams.plugins.notification

import scala.actors.Actor
import scala.actors.Actor._
import scala.collection.mutable.Map
import scala.collection.jcl.Conversions._

import net.sf.json.JSONObject
import com.rabbitmq.client.{Connection, ConnectionFactory, ConnectionParameters}
import com.rabbitmq.client.{QueueingConsumer}

import com.rabbitmq.streams.harness.Server

class NotificationServer() extends Server() {

  private val terms: Map[String, Receiver] = Map()

  case class Notification(terminal: String, message: Array[Byte])

  object Publisher extends Actor {
    def act {
      loop {
        react {
          case Notification(terminal, msg) =>
            publishToDestination(msg, terminal) // FIXME and headers!
        }
      }
    }
    start
  }

  class Receiver(terminal: String, bindingKeys: Seq[String], conn: Connection) extends Actor {
    override def act {
//      val channel = conn.createChannel
//      val queue = channel.queueDeclare().getQueue
//      bindingKeys.foreach(bk => channel.queueBind(queue, "feedshub/log", bk))
//      val consumer = new QueueingConsumer(channel)
//      channel.basicConsume(queue, true, consumer);
//      log.debug("Starting listener for " + terminal + " using " + bindingKeys + " on queue" + queue)
//
//      loop {
//        try {
//          val delivery = consumer.nextDelivery
//          log.debug("New delivery: " + new String(delivery.getBody))
//          Publisher ! new Notification(terminal, delivery.getBody)
//        }
//        catch {
//          case e: InterruptedException =>; // try again
//        }
//      }
    }

  }

  override def terminalStatusChange(terminalId: String, configs: java.util.List[JSONObject], active: Boolean) {
    def maybeStopTerminal(id: String) {
      terms.removeKey(id) match {
        case Some(t) => t.exit
        case None =>;
      }
    }

    if(active) {
      maybeStopTerminal(terminalId)
//      val receiver = new Receiver(terminalId, configs.map(conf => conf.getJSONObject("source").getString("bindingkey")), connection)
//      terms += (terminalId -> receiver)
//      receiver.start
//      log.debug("Start listening to " + terminalId)
//      log.debug("Now listening to " + terms)
    }
    else {
      maybeStopTerminal(terminalId)
      log.debug("Stop listening to " + terminalId)
    }
  }


}
