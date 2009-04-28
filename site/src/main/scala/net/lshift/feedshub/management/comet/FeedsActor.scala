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

    override def render : RenderOut = {
        Console.println("Bind happens")
        bind("list" ->
            (<ul>
                {feeds.map(f => <li>{f.id}<span class="active">{f.active}</span></li>)}
            </ul>))
    }

    override def localSetup {
        Console.println("Local setup")
        Feeds ! AddListener(this)
    }

    override def localShutdown {
        Console.println("Local shutdown")
        Feeds ! RemoveListener(this)
    }

    override def lowPriority : PartialFunction[Any, Unit] = {
        case UpdateFeedList(newList) => feeds = newList; reRender(false)
    }
}
