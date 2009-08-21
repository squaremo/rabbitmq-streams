package com.rabbitmq.streams.management.snippet

import java.text.{ParseException, SimpleDateFormat}
import java.util.{Calendar, Date}
import model.LocalServer
import net.liftweb.http.{RequestVar, SHtml, S}
import scala.xml._
import net.liftweb.util.Helpers._

class Archives {
  object archiveName extends RequestVar[String]("")
  object filterPeriod extends RequestVar[(Date, Date)]((asDate(S.param("start").openOr("")), asDate(S.param("end").openOr(""))))
  object filtered extends RequestVar[Boolean](false)
  object terminalQuery extends RequestVar[String]("")

  private def fiveMinutesPrevious: (String, String, String, String) =  {
    val calendar = Calendar.getInstance
    calendar.setTime(new Date)
    val now = formatDate(calendar.getTime)

    calendar.roll(Calendar.MINUTE, -5)
    val then = formatDate(calendar.getTime)

    (then._1, then._2, now._1, now._2)
  }

  private def formatDate(date: Date): (String, String) =  {
    val dateFormatter = new SimpleDateFormat("dd/MM/yyyy")
    val timeFormatter = new SimpleDateFormat("HH:mm")

    (dateFormatter.format(date), timeFormatter.format(date))
  }

  private def filterValues: (String, String, String, String) = {
    filtered.is match {
      case true => {
        val start = formatDate(filterPeriod.is._1)
        val end = formatDate(filterPeriod.is._2)
        (start._1, start._2, end._1, end._2)
      }
      case false => fiveMinutesPrevious
    }
  }

  var (startDate, startTime, endDate, endTime) = filterValues

  def archives(content: NodeSeq): NodeSeq = {
    val archives = LocalServer.archives

    bind("archives", content,
      "list" -> archives.flatMap(archive =>
      bind("t", chooseTemplate("tag", "list", content),
        "name" -> Text(archive.name),
        bindFilterForm(content, archive.name),
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

  private def bindFilterForm(content: NodeSeq, archive: String): BindParam = {
    def filterForDatabase(database: String): Function0[Unit] = {
      return new Function0[Unit]() {
        override def apply(): Unit = {
          S.redirectTo("/archive/browse", () => {
            archiveName(database)
            filterPeriod(asInterval(startDate, startTime, endDate, endTime))
            filtered(true)
          })
        }
      }
    }

    "filter" -> bind("f", chooseTemplate("tag", "form", content),
      "startDate" -> SHtml.text(startDate, startDate = _, ("id", "startDate")),
      "startTime" -> SHtml.text(startTime, startTime = _, ("id", "startTime")),
      "endDate" -> SHtml.text(endDate, endDate = _, ("id", "endDate")),
      "endTime" -> SHtml.text(endTime, endTime = _, ("id", "endTime")),
      "submit" -> SHtml.submit("Filter", filterForDatabase(archive))
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
    LocalServer.byName(archiveName.is) match {
      case None => content
      case Some(archive) => {
        val period = filterPeriod.is
        val entries =filtered.is match  {
          case false => archive.latestEntries(20)
          case true => archive.entries(period._1, period._2)._1
        }
        bind("archive", content,
          "name" -> Text(archiveName.is),
          "from" -> Text(period._1.toString),
          "to" -> Text(period._2.toString),
          bindFilterForm(content, archiveName.is),
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
    
    bind("results", content,
      "query" -> Text(terminalQuery.is),
      "results" -> terminals.flatMap(terminal =>
        bind("t", chooseTemplate("tag", "terminal", content),
          "name" -> Text(terminal.name),
          terminal.archived match {
            case true  => "archive" -> SHtml.link("/archive/browse", () => {archiveName(terminal.archiveName); println("Bound archive name")}, Text(terminal.archiveName))
            case false => "archive" -> Text("Not archived")
          } 
        )
      )
    )
  }
}
