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

    override def bindingKey : String = "*" // only things with one component

    val StatusView = "feeds/all"

    var feedMap = Map[String, Boolean]() // temp because we'll get this from couch

    def feeds : List[FeedStatus] =
        feedMap.projection.map { case (id, active) => new FeedStatus(id, active)} toList

    def readFeedMap {         
        val vr = statusDb.queryView(StatusView, classOf[Boolean], null, null).getRows
        feedMap = Map(vr.map(v => (v.getKey.toString -> v.getValue)):_*)
    }

    def changedStatus(feedid: String) {
        readFeedMap // TODO for now, just reload everything
    }

    def statusDocName(id : String) : String = id + "_status"

    def updateStatusDocument(feedid: String, newStatus: boolean) {
        // TODO put in couch
        // TODO: this should be indirect, in the sense that we should listen to
        // the command exchange and rearead our map when we get a status change
        val doc = statusDb.getDocument(classOf[java.util.Map[String, Object]], statusDocName(feedid))
        doc.put("active", boolean2Boolean(newStatus))
        statusDb.updateDocument(doc)
    }

    def sendStatusChange(feedid: String) {
        channel.basicPublish(ConfigExchange, feedid,
                             MessageProperties.PERSISTENT_TEXT_PLAIN,
                             StatusChangeMsgBin)
    }
    
    def updateFeedStatus(newStatus: boolean)(feedid: String) {
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
        case ListFeeds() => reply(UpdateFeedList(feeds)) // FIXME do we need this?
        case AddFeed(dfn) =>
            // put the definition in CouchDB
            notifyOfUpdate
        case StartFeed(id) => startFeed(id); notifyOfUpdate
        case StopFeed(id) => stopFeed(id); notifyOfUpdate
        case StatusChange(id) =>
            changedStatus(id); notifyOfUpdate
        case ConfigChange(id) =>
            true // ignore for the minute
    }

    def act = {
        loop {
            react (handleCommands orElse handleObservers)
        }
    }

    readFeedMap
    start
}
