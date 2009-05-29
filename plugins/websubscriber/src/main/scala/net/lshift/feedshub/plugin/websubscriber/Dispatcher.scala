/*
 * Dispatcher.scala
 */

package net.lshift.feedshub.plugin.websubscriber

import scala.actors.Actor
import scala.actors.Actor._

import net.sf.json.JSONObject
import com.fourspaces.couchdb.Session
import net.lshift.feedshub.harness._
import com.rabbitmq.client.QueueingConsumer.Delivery;

import scala.collection.mutable.Map

case class StatusChange(subscription: String, configs: Seq[JSONObject], active: Boolean)

class Dispatcher(log : Logger, couch : Session) extends Actor {
    private val subscriptions : Map[String, List[Subscription]] = Map()

    def publish(message : String) {
        log.debug("Publishing: " + message)
    }

    def act() {
        loop {
            react {
                case StatusChange(id, configs, active) => {
                        if (active) {
                            log.info("Activating " + id)
                            if (! subscriptions.contains(id)) {
                                val subs = configs.map(config =>
                                    new Subscription(log, stateFromConfig(config), saveSubscriptionState(id) _, publish _))
                                subscriptions += (id -> List(subs:_*))
                                subs.foreach(_.start)
                            }
                            else log.warn(id + " already active")
                        }
                        else {
                            log.info("Deactivating " + id)
                            subscriptions.get(id) match {
                                case Some(subs) => {
                                        subscriptions -= id
                                        subs.foreach(_ ! StopPolling)
                                    }
                                case None => log.info(id + " not known or not active")
                            }
                        }
                        log.debug("Now listening to :" + subscriptions.toString)
                    }
            }
        }
    }

    def stateFromConfig(config : JSONObject) : State = {
        // Get the configuration, and go look for any current state
        // Check the original URL against the configured one
        // Make a state documment and save it
        val sourceConfig = config.getJSONObject("source")
        val url = sourceConfig.getString("url")
        val interval = sourceConfig.getInt("interval")
        new State(url, url, 0, interval, PollResult.OK)
    }

    def saveSubscriptionState(id : String)(state : State) {
        log.debug("Saving state : " + state)
        // TODO Save to the correct place in the DB
    }
}
