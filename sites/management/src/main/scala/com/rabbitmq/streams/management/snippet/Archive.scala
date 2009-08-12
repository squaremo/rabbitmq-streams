package com.rabbitmq.streams.management.snippet


import net.liftweb.http.SHtml
import net.liftweb.util.Helpers._
import scala.xml.NodeSeq

import net.liftweb.widgets.tablesorter.TableSorter

import com.rabbitmq.streams.management.controller._
import com.rabbitmq.streams.management.model.LocalServer

class Archive {

  def table: NodeSeq = {
    val archives = LocalServer.archives
    <head>
      <style type="text/css">#archive-list {{width: 600px;}}</style>
      <script type="text/javascript" src="/js/archive.js"></script>
    </head>
    <div>
      {archives.map(a =>
          <h3>{a.name} - {a.terminalName}</h3>
          <form>
            <input id="fromDate" type="text"/>
            <input id="fromTime" type="text"/>
            <input id="toDate" type="text"/>
            <input id="toTime" type="text"/>
            <input type="submit" value="Filter archive"/>
          </form>

          <table>
            <thead>
              <tr><th>Updated</th><th>Content</th></tr>
            </thead>
            <tbody>
              {a.entries(10).map(e => <tr><td>{e.updated}</td><td>{e.content}</td></tr>)}
            </tbody>
          </table>)
      }
    </div>

  }
}
