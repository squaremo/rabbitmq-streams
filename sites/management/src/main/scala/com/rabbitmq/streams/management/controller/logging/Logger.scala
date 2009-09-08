package com.rabbitmq.streams.management.controller.logging

import bootstrap.liftweb.Boot
import scala.actors.Actor
import scala.actors.Actor._
import scala.collection.mutable.HashSet
import com.rabbitmq.client._
import com.rabbitmq.streams.management.utils.actor.Stop

/**
 * This is an abstract class used to consume messages from the feeshub log exchange.
 *
 * The exact log messages to be consumed need to defined using a LogBinding.
 *
 */
abstract class Logger(binding: LogBinding) extends Actor {
  val LogExchange = "feedshub/log"

  def connect {
    val listener = this
    val parameters = new ConnectionParameters
    parameters.setUsername(Boot.rabbitConfig.username)
    parameters.setPassword(Boot.rabbitConfig.password)
    parameters.setVirtualHost("/")
    val cf = new ConnectionFactory(parameters)
    val ch = cf.newConnection(Boot.rabbitConfig.server, Boot.rabbitConfig.port).createChannel

    object LogConsumer extends DefaultConsumer(ch) {
      override def handleDelivery(consumerTag: String, envelope: Envelope, properties: AMQP.BasicProperties, body: Array[Byte]) = {
        val key = envelope.getRoutingKey
        val logLevel = LogLevel.from(key.substring(0, key.indexOf(".")))
        val component = key.split("\\.").drop(1)
        listener ! LogMessage(logLevel, new String(body), component)
      }
    }

    val queue = ch.queueDeclare().getQueue
    binding.bindings.foreach(ch.queueBind(queue, LogExchange, _))
    ch.queueBind(queue, LogExchange, "#")
    ch.basicConsume(queue, true, LogConsumer)
  }

  /**
   * Override this definition to handle additional messages
   *
   * NB. don't forget to call this handler at the end of the over-ridden handler like this:
   *
   * <code>localHandlers orElse super.handlers</code>
   */
  def handlers: PartialFunction[Any, Unit] = {
    case msg@LogMessage(level, desc, component) => processMessage(msg)
    case Stop => exit("stop")
  }

  def act() = {
    loop(react(handlers))
  }

  def processMessage(message: LogMessage)

  connect
  start
}

case class LogMessage(level: LogLevel, msg: String, component: Array[String])
