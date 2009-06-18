package com.rabbitmq.streams.management.snippet


import liftweb.widgets.tablesorter.TableSorter
import xml.NodeSeq

class FeedMonitor {
  def table: NodeSeq = {
    <table>
      <thead>
        <tr><th>Feed</th><th>Last log message</th></tr>
      </thead>
      <tbody>
        <tr><td>Component</td><td>Last log messaage</td></tr>
      </tbody>
    </table>
//    <head>
//    <style type="text/css">#archive-list {{width: 600px;}}</style>
//    </head>
//    <div>{TableSorter.renderOnLoad("archive-list")}
//      <lift:comet type="ArchivesActor">
//        <a:list>Loading...</a:list>
//      </lift:comet>
//    </div>
  }
}
