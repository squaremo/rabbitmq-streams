package com.rabbitmq.streams.management.snippet


import net.liftweb.http.SHtml
import net.liftweb.util.Helpers._
import net.liftweb.util.Log._
import scala.xml.NodeSeq

import net.liftweb.widgets.tablesorter.TableSorter

import com.rabbitmq.streams.management.controller._
import com.rabbitmq.streams.management.model.LocalServer
import scala.xml._
import net.liftweb.http.S
import net.liftweb.http.S.SFuncHolder

class Archive {

  def archives(content: NodeSeq): NodeSeq = {
    var startDate = ""
    var startTime = ""
    var endDate = ""
    var endTime = ""
    var database = ""
    val archives = LocalServer.archives

    def filterForDatabase(database:String): Function0[Unit] = {
      return new Function0[Unit]()  {
        override def apply():Unit = {
          println("\n*\n*\n*     DATABASE IS " + database)
        }
      }
    }

    bind("archives", content,
      "list" -> archives.flatMap(archive =>
        bind("t", chooseTemplate("tag", "list", content),
          "name" -> Text(archive.name),
          "filter" -> bind("f", chooseTemplate("tag", "form", content),
            "startDate" -> SHtml.text(startDate, startDate = _, ("id", "startDate")),
            "startTime" -> SHtml.text(startTime, startTime = _, ("id", "startTime")),
            "endDate" -> SHtml.text(endDate, endDate = _, ("id", "endDate")),
            "endTime" -> SHtml.text(endTime, endTime = _, ("id", "endTime")),
            "submit" -> SHtml.submit("Filter", filterForDatabase(archive.name))
           ),
          "entries" -> archive.entries(10).flatMap(entry =>
            bind("e", chooseTemplate("tag", "entry", content),
              "updated" -> Text(entry.updated.toLocaleString),
              "content" -> Text(entry.content)
            )
          )
        )
      )
    )  
  }

  private def filter(content: NodeSeq, archive:String): NodeSeq = {


//
//    def filter() {
//      debug("Content is " + content.toString)
//      debug("Filtering archive -" + archive)
//    }
//
//    bind("filter", content,
//         "startDate" -> SHtml.text(startDate, startDate = _),
//         "startTime" -> SHtml.text(startTime, startTime = _),
//         "endDate" -> SHtml.text(endDate, endDate = _),
//         "endTime" -> SHtml.text(endTime, endTime = _),
//         "archive" -> SHtml.hidden(() => println()),
//         "submit" -> SHtml.submit("Filter", filter)
//    )
    content
  }

  
}
