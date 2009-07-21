/*
 * Dispatcher.scala
 */

package com.rabbitmq.streams.plugin.websubscriber

import scala.actors.Actor
import scala.actors.Actor._

import net.sf.json.JSONObject
import com.rabbitmq.streams.harness._

import net.liftweb.util.Helpers.hexDigest

import scala.collection.mutable.Map

case class StatusChange(subscription: String, configs: Seq[JSONObject], active: Boolean)

class Dispatcher(log : Logger, publish: (String, String) => Unit, couch : DatabaseResource) extends Actor {
    private val subscriptions : Map[String, List[Subscription]] = Map()

    def publishAsTerminal(terminalId : String)(message : String) {
        publish(message, terminalId)
    }

    def act() {
        loop {
            react {
                case StatusChange(id, configs, active) => {
                        if (active) {
                            log.info("Activating " + id)
                            if (! subscriptions.contains(id)) {
                                val subs = configs.map(config => {
                                        val sourceConfig = config.getJSONObject("source")
                                        val name = subscriptionStateName(id, sourceConfig)
                                    new Subscription(log, stateFromConfig(name, sourceConfig),
                                                     saveSubscriptionState(name) _,
                                                     publishAsTerminal(id) _)
                                })
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
                                        subs.foreach(_ ! StopPolling("deactivated"))
                                    }
                                case None => log.info(id + " not known or not active")
                            }
                        }
                        log.debug("Now listening to :" + subscriptions.keySet toString)
                    }
            }
        }
    }

    private def populateDocFromState(doc : JSONObject, state : State) {
        doc.put("currentUrl", state.currentUrl)
        doc.put("originalUrl", state.originalUrl)
        doc.put("etag", state.etag getOrElse null)
        doc.put("lastModified", state.lastModified getOrElse 0)
        doc.put("lastUpdated", state.lastUpdated)
        doc.put("lastResult", state.lastResult.toString)
    }

    // Merge the state from a document with a config.
    // Prefer the document's values -- this is used when starting a Subscription,
    // so it will either be a blank document or from the last time it was started.
    private def stateFromDoc(stateDoc : JSONObject, config : JSONObject) : State = {
        new State(stateDoc.optString("currentUrl", config.getString("url")),
                  config.getString("url"),
                  stateDoc.optLong("lastUpdated", 0),
                  config.getInt("interval"),
                  PollResult.valueOf(stateDoc.optString("lastResult", "")) getOrElse PollResult.Unknown,
                  stateDoc.optString("etag", null) match {case null => None; case etag => Some(etag)},
                  stateDoc.optLong("lastModified", 0) match {case 0 => None; case lm => Some(lm)})
    }

    def stateFromConfig(name : String, config : JSONObject) : State = {
        val stateDoc = getSubscriptionStateDoc(name)
        stateFromDoc(stateDoc, config)
    }

    def subscriptionStateName(terminalId: String, config : JSONObject) : String = {
        // We don't have a way of identifying individual pollers,
        // except by the URL they are supposed to poll.
        val url = config.getString("url")
        val extra = hexDigest(url.getBytes)
        terminalId + extra
    }

    def getSubscriptionStateDoc(docName : String) : JSONObject = {
        couch.getDocument(docName) match { // FIXME don't rely on _idxs
            case null => {val d = new JSONObject; d.put("_id", docName); d}
            case doc => doc
        }
    }

    def saveSubscriptionState(docName : String)(state : State) {
        log.debug("Saving state : " + state)
        val doc = getSubscriptionStateDoc(docName)
        populateDocFromState(doc, state)
        couch.saveDocument(doc, docName)
    }
}
