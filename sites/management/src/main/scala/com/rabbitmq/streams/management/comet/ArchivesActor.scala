package com.rabbitmq.streams.management.comet


import net.liftweb.http._
import net.liftweb.util.Full
import net.liftweb.util.ActorPing
import net.liftweb.util.Helpers.TimeSpan
import net.liftweb.http.js.JsCmds.SetHtml
import net.liftweb.widgets.tablesorter.TableSorter
import model.{Archive, LocalServer}
import scala.xml._

class ArchivesActor extends CometActor {

  var archives: List[Archive] = Nil

  override def defaultPrefix = Full("a")



  override def render: RenderOut = {
    Console.err.println("Rendering archive")
    archives = LocalServer.archives.toList
    bind("list" ->(
        <div>
          {archives.map(a =>
              <h3>{a.name} - {a.terminalName}</h3>
              <table>
                <thead>
                  <tr><th>Updated</th><th>Content</th></tr>
                </thead>
                <tbody>
                  {a.entries(10).map(e => <tr><td>{e.updated}</td><td>{e.content}</td></tr>)}
                </tbody>
              </table>)
          }
        </div>))
  }

  override def lowPriority : PartialFunction[Any, Unit] = {
    case Again => {
        partialUpdate(SetHtml("list", Text("timeNow.toString")))
        ActorPing.schedule(this, Again, TimeSpan(10000L)) // schedule an update in 10 seconds
      }
  }

  override protected def localSetup() = {
    //    archives = LocalServer.archives.toList
  }

  case object Again
}
