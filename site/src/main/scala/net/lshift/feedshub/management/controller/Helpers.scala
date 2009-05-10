/*
 * Helpers.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.lshift.feedshub.management.controller

import scala.actors.Actor
import scala.collection.mutable.Set

import com.rabbitmq.client._
import org.jcouchdb.db.Database
import org.jcouchdb.document.{BaseDocument, ViewResult, ValueRow}
import org.jcouchdb.document.ViewResult
import org.svenson.JSONProperty

trait ConfigListener {
    val ConfigExchange = "feedshub/config"

    def bindingKey : String
    val channel : Channel

    def handleConfigChange(routingKey : String, message: String) : Unit

    def subscribeToConfigChanges() {
        object ConfigConsumer extends DefaultConsumer(channel) {
            override def handleDelivery(consumerTag : String,
                                              envelope : Envelope,
                                              properties : AMQP.BasicProperties,
                                              body : Array[Byte]) = {
                      val tag = envelope.getDeliveryTag
                      ConfigListener.this.handleConfigChange(envelope.getRoutingKey, new String(body))
                      channel.basicAck(tag, false)
                  }
        }

        val queue = channel.queueDeclare().getQueue
        //Console.println("Binding "+queue+" to "+ConfigExchange)
        channel.queueBind(queue, ConfigExchange, bindingKey)
        channel.basicConsume(
            queue, // queue name
              false, // that's a negatory on, um, not acking ..
              ConfigConsumer
            )
    }

}

abstract class Change
case class ConfigChange(id: String) extends Change
case class StatusChange(id: String) extends Change

trait ConfigAwareActor extends ConfigListener {
    def !(msg : Any) : Unit

    def bindingKey : String = "#" // anything

    def handleConfigChange(routingKey : String, message: String) {
        message match {
            case "status change" => this ! StatusChange(routingKey)
            case "config change" => this ! ConfigChange(routingKey)
        }
    }
}

trait FeedsHubConfig {
    val StatusDb = "feedshub_status"

    val StatusChangeMsgBin = "status change".getBytes

    def newchannel : Channel = {
        val parameters = new ConnectionParameters
        parameters.setUsername("feedshub_admin")
        parameters.setPassword("feedshub_admin")
        parameters.setVirtualHost("/")
        val cf = new ConnectionFactory(parameters)
        cf.newConnection("localhost", 5672).createChannel
    }

    val channel = newchannel

    val statusDb : Database = new Database("localhost", 5984, StatusDb)
}

case class Observe(observer : Actor)
case class Unobserve(observer : Actor)

trait ObservableActor[M] {
    private val listeners : Set[Actor] = Set()

    protected def registerObserver(observer : Actor) {
        listeners.incl(observer)
    }

    protected def unregisterObserver(observer : Actor) {
        listeners.excl(observer)
    }

    protected def notifyObservers(message : M) {
        listeners.foreach(_ ! message)
    }

    protected def newObserver(newbie : Actor)

    protected val handleObservers : PartialFunction[Any, Unit] = {
        case Observe(obs) => registerObserver(obs); newObserver(obs)
        case Unobserve(obs) => unregisterObserver(obs)
    }

}