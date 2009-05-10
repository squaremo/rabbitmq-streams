/*
 * FeedsActor.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.lshift.feedshub.management.comet

import scala.collection.mutable.HashMap
import scala.xml.{NodeSeq, Text}

import net.liftweb.http._
import net.liftweb.http.js.JsCmds.Noop
import net.liftweb.util.{Box, Full}

import net.lshift.feedshub.management.controller._

class FeedsActor extends CometActor {

    var feeds : List[FeedStatus] = Nil

    override def defaultPrefix = Full("f")

    def feedControl(feed : FeedStatus) : NodeSeq = {
        def sendCommand(msg : FeedsCmd) = {
            Feeds ! msg
            Noop
        }
        if (feed.active)
            SHtml.ajaxButton("Stop", () => sendCommand(StopFeed(feed.id)), "class" -> "blah")
        else
            SHtml.ajaxButton("Start", () => sendCommand(StartFeed(feed.id)))
    }

    override def render : RenderOut = {
        bind("list" ->
            (<ul class="data-source-list">
                {feeds.map(f => <li><span>{f.id}</span><span class="ctrl">{feedControl(f)}</span></li>)}
            </ul>))
    }

    override def localSetup {""
        Console.println("Local setup")
        Feeds ! AddListener(this)
    }

    override def localShutdown {
        Feeds ! RemoveListener(this)
    }

    override def lowPriority : PartialFunction[Any, Unit] = {
        case UpdateFeedList(newList) => feeds = newList; reRender(false)
    }
}
