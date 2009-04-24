/*
 * A
 *
 */

package net.lshift.feedshub.management.controller

import scala.collection.mutable.HashSet
import net.liftweb.util.Helpers._
import scala.actors.Actor
import scala.actors.Actor._

abstract class FeedsCmd
case class AddListener(listener: Actor) extends FeedsCmd
case class RemoveListener(listener: Actor) extends FeedsCmd
case class ListFeeds() extends FeedsCmd
case class UpdateFeedList(feeds: List[Feed]) extends FeedsCmd
case class Init(couchUrl : String) extends FeedsCmd
case class AddFeed(definition: FeedDefinition) extends FeedsCmd

/**
* Encapsulates a single feed.  This subscribes to notifications for the feed,
* and updates its listeners correspondingly.
*/
class Feed(id : String) {
}

class FeedDefinition

/**
 * Singleton registry of feeds.  Copes with adding and removing feeds.
 */
object Feeds extends Actor {
    val listeners = new HashSet[Actor]
    var tempFeedList = List[Feed]() // temp because we'll get this from couch

    def feeds : List[Feed] =
        tempFeedList

    def notifyListeners =
        for (listener <- listeners)
            listener ! UpdateFeedList(feeds)

    def act = {
        loop {
            react {
                case Init(url) =>
                    // go and fetch from CouchDB
                    notifyListeners
                case AddListener(listener: Actor) =>
                    listeners.incl(listener)
                    listener ! UpdateFeedList(feeds)
                case RemoveListener(listener: Actor) =>
                    listeners.excl(listener)
                case ListFeeds() => reply(UpdateFeedList(feeds))
                case AddFeed(dfn) =>
                    // put the definition in CouchDB
                    tempFeedList = new Feed("foo") :: tempFeedList
                    notifyListeners
            }
        }
    }
    
    start
}
