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

import net.liftweb.widgets.tablesorter.TableSorter

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
            SHtml.ajaxButton("Stop", () => sendCommand(StopFeed(feed.id)), "class" -> "control-button")
        else
            SHtml.ajaxButton("Start", () => sendCommand(StartFeed(feed.id)), "class" -> "control-button")
    }

    override def render : RenderOut = {
        bind("list" ->
             (<table id="feeds-list" class="tablesorter">
                <thead>
                       <tr><th>Feed</th><th>Status</th><th></th></tr>
                </thead>
                <tbody>
                       {feeds.map(f =>
                  <tr>
                      <td>{f.id}</td>
                      <td class="status">{if (f.active) "Active" else "Inactive"}</td>
                      <td class="ctrl">{feedControl(f)}</td>
                  </tr>)}
                </tbody>
              </table>
              <script>
                {TableSorter.jsRender("feeds-list")}
              </script>))
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
