package com.rabbitmq.streams.management.snippet


import net.liftweb.http.SHtml
import net.liftweb.util.Helpers._
import scala.xml.NodeSeq

import net.liftweb.widgets.tablesorter.TableSorter

import com.rabbitmq.streams.management.controller._

class Archive {

  def table: NodeSeq = {
    <head>
    <style type="text/css">#archive-list {{width: 600px;}}</style>
    </head>
    <div>{TableSorter.renderOnLoad("archive-list")}
      <lift:comet type="ArchivesActor">
        <a:list>Loading...</a:list>
      </lift:comet>
    </div>
  }
}
