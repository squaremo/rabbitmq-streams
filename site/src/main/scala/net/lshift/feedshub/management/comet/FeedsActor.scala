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
import net.liftweb.util.Can

import net.lshift.feedshub.management.controller._

class FeedsActor(theSession: LiftSession, name: Can[String],
                defaultXml: NodeSeq, attributes: Map[String, String])
    extends CometActor(theSession, name, defaultXml, attributes) {

    var feeds : List[Feed] = Nil

    def defaultPrefix = "feeds"

    def render = {
        <ul>
            {feeds.map(f => <li>Feed</li>)}
        </ul>
    }

    override def localSetup {
        Feeds !? AddListener(this) match {
            case UpdateFeedList(feedlist) => feeds = feedlist
        }
    }

    override def localShutdown {
        Feeds ! RemoveListener(this)
    }

    override def lowPriority : PartialFunction[Any, Unit] = {
        case UpdateFeedList(newList) => feeds = newList; reRender(false)
    }
}
