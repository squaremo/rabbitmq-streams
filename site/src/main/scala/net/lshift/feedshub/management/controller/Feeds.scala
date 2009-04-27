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

// TODO can we make this a bit nicerer
class FeedStatus extends BaseDocument {
    var active = false

    @JSONProperty{val value="active"}
    def getActive : boolean = active
    def setActive(value: boolean) : Unit = {
        active = value
    }
}

abstract class FeedsCmd
case class AddListener(listener: Actor) extends FeedsCmd
case class RemoveListener(listener: Actor) extends FeedsCmd
case class ListFeeds() extends FeedsCmd
case class UpdateFeedList(feeds: List[FeedStatus]) extends FeedsCmd
case class Init(couchUrl : String) extends FeedsCmd
case class AddFeed(definition: FeedDefinition) extends FeedsCmd

class FeedDefinition

/**
 * Singleton registry of feeds.  Copes with adding and removing feeds.
 * TODO factor out the command channel
 */
object Feeds extends Actor {
    val listeners = new HashSet[Actor]
    var tempFeedList = List[FeedStatus]() // temp because we'll get this from couch

    var channel : Option[Channel] = None
    var statusDb : Option[Database] = None

    def feeds : List[FeedStatus] =
        tempFeedList

    def notifyListeners =
        for (listener <- listeners)
            listener ! UpdateFeedList(feeds)

    def initFromCouch(url: String) : Unit = {
        // TODO get from couch
        // TODO set up couch databases

        // set up a command channel
        val parameters = new ConnectionParameters
        parameters.setUsername("feedshub_admin")
        parameters.setPassword("feedshub_admin")
        parameters.setVirtualHost("/")
        val cf = new ConnectionFactory(parameters)
        channel = Some(cf.newConnection("localhost", 5672).createChannel)

        // TODO set up the feed status database properly from URL
        val db = new Database("localhost", 5984, "feedshub_status")

        // Get our list
        val vr = db.queryView("feeds", classOf[FeedStatus], null, null).getRows
        tempFeedList = vr.map(v => v.getValue).toList
    }

    def updateStatusDocument(feedid: String, newStatus: boolean) : Unit = {
        // put in couch
    }

    def sendStatusChange(feedid: String) : Unit = {
        // put in couch
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
                case ListFeeds() => reply(UpdateFeedList(feeds))
                case AddFeed(dfn) =>
                    // put the definition in CouchDB
                    
                    notifyListeners
            }
        }
    }
    
    start
}
