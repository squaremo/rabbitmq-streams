/*
 * Classes for managing feeds.
 *
 */

package net.lshift.feedshub.management.controller

import scala.collection.mutable.HashSet
import scala.collection.jcl.Conversions._
import net.liftweb.util.Helpers._
import scala.actors.Actor
import scala.actors.Actor._
import com.rabbitmq.client._
import org.jcouchdb.db.Database
import org.jcouchdb.document.{BaseDocument, ViewResult, ValueRow}
import org.jcouchdb.document.ViewResult
import org.svenson.JSONProperty

// This should go in the model
case class FeedStatus(id: String, active: Boolean) {
    // case class so we get the handy accessors
}

abstract class FeedsCmd
case class AddListener(listener: Actor) extends FeedsCmd
case class RemoveListener(listener: Actor) extends FeedsCmd
case class ListFeeds() extends FeedsCmd
case class UpdateFeedList(feeds: List[FeedStatus]) extends FeedsCmd
case class Init(couchUrl : String) extends FeedsCmd
case class AddFeed(definition: FeedDefinition) extends FeedsCmd
case class StartFeed(feedid: String) extends FeedsCmd
case class StopFeed(feedid: String) extends FeedsCmd
case class ConfigChange(feedid: String, bondy: Array[Byte]) extends FeedsCmd

class FeedDefinition

/**
 * Singleton registry of feeds.  Copes with adding and removing feeds.
 * TODO factor out the command channel
 */
object Feeds extends Actor {
    val StatusDb = "feedshub_status"
    val ConfigExchange = "feedshub/config"
    val StatusView = "feeds/all"
    val StatusChange = "status change"
    val StatusChangeMsg = configMessageEncode(StatusChange)

    val listeners = new HashSet[Actor]
    var feedMap = Map[String, Boolean]() // temp because we'll get this from couch

    var channel : Option[Channel] = None
    var statusDb : Option[Database] = None

    def feeds : List[FeedStatus] =
        feedMap.projection.map { case (id, active) => new FeedStatus(id, active)} toList

    def notifyListeners {
        val fs = feeds
        for (listener <- listeners)
            listener ! UpdateFeedList(fs)
    }

    // The orchestrators interpret config messages as ASCII strings
    def configMessageEncode(msg: String) : Array[Byte] = msg.getBytes("US-ASCII")
    def configMessageDecode(body: Array[Byte]) = new String(body, "US-ASCII")

    def readFeedMap {
        // TODO set up the feed status database properly from URL
        statusDb match {
            case Some(db) =>
                // Get our list
                val vr = db.queryView(StatusView, classOf[Boolean], null, null).getRows
                feedMap = Map(vr.map(v => (v.getKey.toString -> v.getValue)):_*)
        }
    }

    def changedStatus(feedid: String) {
        readFeedMap // TODO for now, just reload everything
    }

    def statusDocName(id : String) : String = id + "_status"

    def initFromCouch(url: String) {
        // TODO get from couch
        // TODO set up couch databases

        // set up a command channel
        val parameters = new ConnectionParameters
        parameters.setUsername("feedshub_admin")
        parameters.setPassword("feedshub_admin")
        parameters.setVirtualHost("/")
        val cf = new ConnectionFactory(parameters)
        val ch = cf.newConnection("localhost", 5672).createChannel
        channel = Some(ch)
        statusDb = Some(new Database("localhost", 5984, StatusDb))
        readFeedMap
        subscribeToStatusChanges(ch)
    }

    def subscribeToStatusChanges(channel : Channel) {
        val listener = this
        object ConfigConsumer extends DefaultConsumer(channel) {
            override def handleDelivery(consumerTag : String,
                                              envelope : Envelope,
                                              properties : AMQP.BasicProperties,
                                              body : Array[Byte]) = {
                      val tag = envelope.getDeliveryTag
                      listener ! ConfigChange(envelope.getRoutingKey, body)
                      channel.basicAck(tag, false)
                  }
        }

        val queue = channel.queueDeclare().getQueue
        //Console.println("Binding "+queue+" to "+ConfigExchange)
        channel.queueBind(queue, ConfigExchange, "#")
        channel.basicConsume(
            queue, // queue name
              false, // that's a negatory on, um, not acking ..
              ConfigConsumer
            )
    }

    def updateStatusDocument(feedid: String, newStatus: boolean) {
        // TODO put in couch
        // TODO: this should be indirect, in the sense that we should listen to
        // the command exchange and rearead our map when we get a status change
        statusDb match {
            case Some(db) =>
                // get the doc, put the doc
                Console.println("Retrieving doc "+statusDocName(feedid))
                val doc = db.getDocument(classOf[java.util.Map[String, Object]], statusDocName(feedid))
                doc.put("active", boolean2Boolean(newStatus))
                db.updateDocument(doc)
        }
    }

    def sendStatusChange(feedid: String) {
        channel match {
            case Some(c) =>
                c.basicPublish(ConfigExchange, feedid,
                     MessageProperties.PERSISTENT_TEXT_PLAIN,
                     StatusChangeMsg)
        }

    }

    def updateFeedStatus(newStatus: boolean)(feedid: String) : Unit = {
        updateStatusDocument(feedid, newStatus)
        sendStatusChange(feedid)
    }
    
    val stopFeed = updateFeedStatus(false) _
    val startFeed = updateFeedStatus(true) _

    def act = {
        loop {
            react {
                case Init(url) =>
                    initFromCouch(url)
                    notifyListeners
                case AddListener(listener: Actor) =>
                    listeners.incl(listener)
                    listener ! UpdateFeedList(feeds)
                case RemoveListener(listener: Actor) =>
                    listeners.excl(listener)
                case ListFeeds() => reply(UpdateFeedList(feeds)) // FIXME do we need this?
                case AddFeed(dfn) =>
                    // put the definition in CouchDB
                    notifyListeners
                case StartFeed(id) => startFeed(id); notifyListeners
                case StopFeed(id) => stopFeed(id); notifyListeners
                case ConfigChange(id, bytes) =>
                        configMessageDecode(bytes) match {
                            case StatusChange => changedStatus(id); notifyListeners
                        }
            }
        }
    }
    
    start
}
