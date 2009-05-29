/*
 * Log.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.lshift.feedshub.management.controller

import scala.collection.mutable.HashSet
import scala.collection.mutable.Queue
import scala.actors.Actor
import scala.actors.Actor._
import com.rabbitmq.client._

// FIXME: Make this an enumeration?
class LogLevel {
    def andUp = LogLevel.downTo(this)
    def andDown = LogLevel.upFrom(this)
    def stringValue : String = {
        this match {
            case Debug => "DEBUG"
            case Info => "INFO"
            case Warn => "WARN"
            case Error => "ERROR"
            case Fatal => "FATAL"
        }
    }
}

object LogLevel extends LogLevel {
    val values = List(Fatal, Error, Warn, Info, Debug)
    def downTo(level : LogLevel) : Set[LogLevel] = {
        Set(values.takeWhile(_ != level):_*) + level
    }
    def upFrom(level: LogLevel) : Set[LogLevel] = {
        Set(values.dropWhile(_ != level):_*)
    }
    def from(s : String) : LogLevel = {
        s.toLowerCase match {
            case "debug" => Debug
            case "info" => Info
            case "warn" => Warn
            case "error" => Error
            case "fatal" => Fatal
        }
    }
}

object Debug extends LogLevel
object Info extends LogLevel
object Warn extends LogLevel
object Error extends LogLevel
object Fatal extends LogLevel

case class LogMessage(level: LogLevel, msg: String, component: Array[String])

case class AddLogListener(downToLevel: LogLevel, listener: Actor)
case class RemoveLogListener(listener : Actor)

case class History(msgs: List[LogMessage])

/**
 * Essentially this is just a class that keeps a rolling buffer of messages,
 * so it can be queried
 */
class Log(size: Int) extends Actor {

    val LogExchange = "feedshub/log"

    val buffer : Queue[LogMessage] = new Queue()

    val listeners = Map(Debug -> HashSet[Actor](),
                        Info -> HashSet[Actor](),
                        Warn -> HashSet[Actor](),
                        Error -> HashSet[Actor](),
                        Fatal -> HashSet[Actor]())

    def enbuffer(msg: LogMessage) {
        buffer.enqueue(msg)
        while (buffer.length > size)
            buffer.dequeue
    }

    def debuffer : LogMessage = {
        if (buffer.isEmpty)
            null
        else
            buffer.front
    }

    def messages(level: LogLevel) : List[LogMessage] = {
        val levels = level.andUp
        buffer.filter(msg => levels.contains(msg.level)).toList
    }

    def notifyListeners(message: LogMessage) {
        for (level <- message.level andDown) {
            listeners(level).foreach(listener => listener ! message)
        }
    }

    def connect {
        val listener = this
        val parameters = new ConnectionParameters
        parameters.setUsername("feedshub_admin")
        parameters.setPassword("feedshub_admin")
        parameters.setVirtualHost("/")
        val cf = new ConnectionFactory(parameters)
        val ch = cf.newConnection("localhost", 5672).createChannel

        object LogConsumer extends DefaultConsumer(ch) {
            override def handleDelivery(consumerTag : String,
                                              envelope : Envelope,
                                              properties : AMQP.BasicProperties,
                                              body : Array[Byte]) = {
                      val key = envelope.getRoutingKey
                      val logLevel = LogLevel.from(key.substring(0, key.indexOf(".")))
                      val component = key.split("\\.").drop(1)
                      listener ! LogMessage(logLevel, new String(body), component)
                  }
        }

        val queue = ch.queueDeclare().getQueue
        //Console.println("Binding "+queue+" to "+ConfigExchange)
        ch.queueBind(queue, LogExchange, "#")
        ch.basicConsume(
            queue, // queue name
              true, // that's a pository on, um, not acking ..
              LogConsumer
            )
    }

    def act = {
        loop {
            react {
                case msg@LogMessage(level, desc, component) =>
                    enbuffer(msg)
                    notifyListeners(msg)
                case AddLogListener(level, listener) =>
                    println("Add log listener " + listener.toString)
                    listener ! History(messages(level))
                    listeners(level) += listener
                case RemoveLogListener(listener) =>
                    listeners.values.foreach(level => level -= listener)
            }
        }
    }

    start
    connect
}
object Log extends Log(100)
