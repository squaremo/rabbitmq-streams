package com.rabbitmq.streams.management.comet


import liftweb.http._
import liftweb.util.Full
import liftweb.widgets.tablesorter.TableSorter
import model.{Archive, LocalServer}
import scala.xml._
class ArchivesActor extends CometActor {

  var archives: List[Archive] = Nil

  override def defaultPrefix = Full("a")

  override def render: RenderOut = {
    bind("list" ->(
      <div>
      {archives.map(a =>
        <h3>{a.name} - {a.title}</h3>
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

  override protected def localSetup() = {
    archives = LocalServer.archives.toList
  }
}
