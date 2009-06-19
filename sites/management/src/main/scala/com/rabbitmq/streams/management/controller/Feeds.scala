/*
 * Classes for managing feeds.
 *
 */

package com.rabbitmq.streams.management.controller

import com.rabbitmq.streams.management.controller.logging.LogMessage
import com.rabbitmq.streams.management.controller.monitoring.FeedMonitors
import scala.collection.mutable.HashSet
import scala.collection.jcl.Conversions._
import net.liftweb.util.Helpers._
import scala.actors.Actor
import scala.actors.Actor._
import com.rabbitmq.client._
import com.fourspaces.couchdb._

// This should go in the model
case class FeedStatus(id: String, active: Boolean) {
    def lastLogMessage: Option[LogMessage] = FeedMonitors.lastLogMessage(id)
}

// For sending to listeners
case class UpdateFeedList(feeds: List[FeedStatus])

abstract class FeedsCmd
case class ListFeeds() extends FeedsCmd
case class Init(couchUrl : String) extends FeedsCmd
case class AddFeed(definition: FeedDefinition) extends FeedsCmd
case class StartFeed(feedid: String) extends FeedsCmd
case class StopFeed(feedid: String) extends FeedsCmd

class FeedDefinition

/**
 * Singleton registry of feeds.  Copes with adding and removing feeds.
 * TODO factor out the command channel
 */
object Feeds extends Actor with FeedsHubConfig with ConfigAwareActor with ObservableActor[UpdateFeedList] {

    override def bindingKey : String = "#" // only things with one component

    val StatusView = "feeds/all"

    var feeds : List[FeedStatus] = Nil // temp because we'll get this from couch

    def readFeedStatus {
        val vr = statusDb.view(StatusView).getResults
        feeds = vr.map(v => FeedStatus(v.getString("key"), v.getBoolean("value"))) toList
    }

    def statusDocName(id : String) : String = id + "_status"

    def updateStatusDocument(feedid: String, newStatus: Boolean) {
        // TODO put in couch
        // TODO: this should be indirect, in the sense that we should listen to
        // the command exchange and rearead our map when we get a status change
        val doc = statusDb.getDocument(statusDocName(feedid))
        doc.put("active", boolean2Boolean(newStatus))
        statusDb.saveDocument(doc)
    }

    def sendStatusChange(feedid: String) {
        channel.basicPublish(ConfigExchange, feedid,
                             MessageProperties.PERSISTENT_TEXT_PLAIN,
                             StatusChangeMsgBin)
    }
    
    def updateFeedStatus(newStatus: Boolean)(feedid: String) {
        updateStatusDocument(feedid, newStatus)
        sendStatusChange(feedid)
    }
    
    val stopFeed = updateFeedStatus(false) _
    val startFeed = updateFeedStatus(true) _

    protected def notifyOfUpdate {
        val list = feeds
        notifyObservers(UpdateFeedList(feeds))
    }

    protected def newObserver(newbie : Actor) {
        newbie ! UpdateFeedList(feeds)
    }

    protected val handleCommands : PartialFunction[Any, Unit] = {
        case AddFeed(dfn) =>
            // put the definition in CouchDB
            notifyOfUpdate
        case StartFeed(id) => startFeed(id)
        case StopFeed(id) => stopFeed(id)
        case StatusChange(id) =>
            println("Status change " + id)
            readFeedStatus; notifyOfUpdate
        case ConfigChange(id) =>
            true // ignore for the minute
    }

    def act = {
        loop {
            react (handleCommands orElse handleObservers)
        }
    }

    readFeedStatus
    start
}
