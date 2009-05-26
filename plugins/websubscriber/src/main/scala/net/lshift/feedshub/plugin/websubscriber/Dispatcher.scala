/*
 * Dispatcher.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.lshift.feedshub.plugin.websubscriber

import scala.actors.Actor
import scala.actors.Actor._

import com.fourspaces.couchdb.Document
import com.fourspaces.couchdb.Session
import net.lshift.feedshub.harness._
import com.rabbitmq.client.QueueingConsumer.Delivery;

import scala.collection.mutable.Map

case class StatusChange(subscription: String, ack : (() => Unit))

class Dispatcher(log : Logger, terminalConfig : String => Document, terminalStatus : String => Document, couch : Session) extends Actor {
    private val subscriptions : Map[String, Subscription] = Map()

    def act {
        loop {
            react {
                case StatusChange(id, ack) => {
                        val status = terminalStatus(id)
                        if (status.getBoolean("active")) {
                            log.debug("Activating " + id)
                            if (! subscriptions.contains(id)) {
                                val sub = new Subscription(log, stateFromConfig(id), saveSubscriptionState(id) _)
                                subscriptions.incl(id -> sub)
                                log.debug("Now listening to :" + subscriptions.toString)
                                sub.start
                            }
                        }
                        else {
                            log.debug("Deactivating " + id)
                            subscriptions.get(id) match {
                                case Some(sub) => {
                                        subscriptions.excl(id) ;
                                        sub ! StopPolling
                                }
                            }
                        }
                        ack()
                    }
            }
        }
    }

    def stateFromConfig(id : String) : State = {
        // Get the configuration, and go look for any current state
        // Check the original URL against the configured one
        // Make a state documment and save it
        val config = terminalConfig(id)
        val url = config.getString("url")
        val interval = config.getInt("interval")
        new State(url, url, 0, interval)
    }

    def saveSubscriptionState(id : String)(state : State) {
        // save the state back to a document
    }
}
