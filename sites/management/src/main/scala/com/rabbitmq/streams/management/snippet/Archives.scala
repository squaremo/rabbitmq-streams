package com.rabbitmq.streams.management.snippet

import java.text.{ParseException, SimpleDateFormat}
import java.util.Date
import model.{Archive, LocalServer}
import net.liftweb.http.{RequestVar, SHtml, S}
import scala.xml._
import net.liftweb.util.Helpers._

class Archives {
  object archiveName extends RequestVar[String]("")
  object filterPeriod extends RequestVar[(Date, Date)]((asDate(S.param("start").openOr("")), asDate(S.param("end").openOr(""))))
  object terminalQuery extends RequestVar[String]("")

  def archives(content: NodeSeq): NodeSeq = {
    var startDate, startTime, endDate, endTime = ""
    val archives = LocalServer.archives

    def filterForDatabase(database: String): Function0[Unit] = {
      return new Function0[Unit]() {
        override def apply(): Unit = {
          S.redirectTo("/archive/browse", () => {
            archiveName(database)
            filterPeriod(asInterval(startDate, startTime, endDate, endTime))
          })
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
        "entries" -> archive.latestEntries(10).flatMap(entry =>
        bind("e", chooseTemplate("tag", "entry", content),
          "updated" -> Text(entry.updated.toLocaleString),
          "content" -> Text(entry.content)
          )
          )
        )
        )
      )
  }

  private def asInterval(startDate: String, startTime: String, endDate: String, endTime: String): (Date, Date) = {
    val start = asDate(startDate.trim + "-" + startTime.trim)
    val end = asDate(endDate.trim + "-" + endTime.trim)
    (start, end)
  }

  private def asDate(date: String): Date = {
    try {
      val parser = new SimpleDateFormat("dd/MM/yyyy-HH:mm")
      parser.parse(date)
    }
    catch {
      case ex: ParseException => new Date
    }

  }

  def browse(content: NodeSeq): NodeSeq = {
    val pageSize = (S.attr("pageSize") openOr "0").toInt
    LocalServer.archive(archiveName.is) match {
      case None => content
      case Some(archive) => {
        val period = filterPeriod.is
        val entries = archive.entries(period._1, period._2)._1
        bind("archive", content,
          "name" -> Text(archiveName.is),
          "from" -> Text(period._1.toString),
          "to" -> Text(period._2.toString),
          "entries" -> entries.flatMap(entry =>
            bind("e", chooseTemplate("tag", "entry", content),
              "updated" -> Text(entry.updated.toLocaleString),
              "content" -> Text(entry.content)
            )
          )
        )
      }
    }
  }

  def terminalSearch(content: NodeSeq): NodeSeq = {
    var query = ""

    def performQuery() {
      S.redirectTo("/archive/terminals", () => { terminalQuery(query)})
    }

    bind("search", content,
      "query" -> SHtml.text(query, query = _),
      "submit" -> SHtml.submit("Find terminal", performQuery)
    )
  }

  def terminalResult(content: NodeSeq): NodeSeq = {
    val terminals = LocalServer.terminals(terminalQuery.is)
    for(t <- terminals) println("TERM IS " + t)
    
    bind("results", content,
      "query" -> Text(terminalQuery.is)
    )
  }
}
