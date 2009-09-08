package com.rabbitmq.streams.management.comet

import scala.collection.mutable.HashMap
import scala.xml.{NodeSeq, Text}

import net.liftweb.http._
import net.liftweb.http.js.JsCmds.Noop
import net.liftweb.util.{Box, Full}

import net.liftweb.widgets.tablesorter.TableSorter

import com.rabbitmq.streams.management.controller._

class FeedsActor extends CometActor {

    var feeds : List[FeedStatus] = Nil

    override def defaultPrefix = Full("f")

    def feedControl(feed : FeedStatus) : NodeSeq = {
        def sendCommand(msg : FeedsCmd) = {
            Feeds ! msg
            Noop
        }
        if (feed.active)
            SHtml.ajaxButton("Stop", () => sendCommand(StopFeed(feed.id)), "class" -> "control-button button-stop")
        else
            SHtml.ajaxButton("Start", () => sendCommand(StartFeed(feed.id)), "class" -> "control-button button-start")
    }

    override def render : RenderOut = {
        bind("list" ->
             (<table id="feeds-list" class="tablesorter">
                <thead>
                       <tr><th>Feed</th><th>Status</th><th>Last log message</th></tr>
                </thead>
                <tbody>
                       {feeds.map(f =>
                  <tr>
                      <td>{f.id}</td>
                      <td class="status">{if (f.active) "Active" else "Inactive"}</td>
                      <td>
                        {SHtml.link("/feed-log", () => println("Show logs for " + f.id), Text(f.lastLogMessage match {
                          case Some(m) => m.level + " " + m.msg
                          case None    => "No log messages available"
                        }), ("feed", f.id))}
                      </td>
                  </tr>)}
                </tbody>
              </table>
              <script>
                {TableSorter.jsRender("feeds-list")}
              </script>))
    }

    override def localSetup {
        Feeds ! Observe(this)
    }

    override def localShutdown {
        Feeds ! Unobserve(this)
    }

    override def lowPriority : PartialFunction[Any, Unit] = {
        case UpdateFeedList(newList) => feeds = newList; reRender(false)
    }
}
