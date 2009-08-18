package com.rabbitmq.streams.management.model

import java.lang.String
import java.net.URL
import java.util.{Calendar, Date}
import scala.collection.jcl.Conversions._
import com.fourspaces.couchdb._
import net.sf.json._

class Server(url: String, configDatabaseName: String) {
  val couch = Server.sessionFromString(url)
  val configDb = couch.getDatabase(configDatabaseName)

  private def newArchive(server: JSONObject, terminal: JSONObject, destination: JSONObject): Archive = {
    new Archive(couch, terminal, destination)
  }

  def archives: Seq[Archive] = {
    for(row <- configDb.view("terminals/byserver?group=true").getResults;
        server = row.getJSONObject("value")
        if server.getJSONObject("server").getString("server_type").equals("archive");
        t <- server.getJSONArray("terminals");
        terminal = t.asInstanceOf[JSONObject];
        destination = terminal.getJSONObject("server").getJSONObject("destination")
    ) yield newArchive(server.getJSONObject("server"), terminal, destination)
  }

  def archive(name: String): Option[Archive] = {
    archives.dropWhile(a => a.name != name).firstOption
  }
}
object Server {
  def sessionFromString(urlStr: String): Session = {
    val url = new URL(urlStr)
    new Session(url.getHost(), url.getPort())
  }
}

object LocalServer extends Server("http://localhost:5984", "feedshub_status") // todo remove hardcoded localhost

/**
 * Represents an archive available as a feed
 */
class Archive(private val couch: Session, private val terminal: JSONObject, private val config: JSONObject) {
  private val dbName: String = "archive_" + terminal.get("_id") + config.get("name")
  val name = config.getString("name")
  val terminalName = terminal.get("_id")

  private val db = couch.getDatabase(dbName)
  private val entriesViewName: String = "by_date/by_date"

  def latestEntries(limit: Int): Seq[ArchiveEntry] = {
    val entriesView = new View(entriesViewName)
    entriesView.setLimit(limit)
    entriesView.setDescending(true)
    require(db != null, "db should not be null for " + dbName)
    performViewQuery(entriesView)._1
  }

  def entries(startKey: String, endKey: String): (Seq[ArchiveEntry], Int) = {
    println("Fetching view for keys " + startKey + " - " + endKey)
    val entriesView = new View(entriesViewName)
    entriesView.setStartKey(startKey)
    entriesView.setEndKey(endKey)
    performViewQuery(entriesView)
  }

  def entries(startDate: Date, endDate: Date) : (Seq[ArchiveEntry], Int) = {
    entries(toViewKey(startDate), toViewKey(endDate))
  }

  private def toViewKey(date: Date): String = {
    val array = new JSONArray()
    val calendar = Calendar.getInstance()
    calendar.setTime(date)

    array.add(calendar.get(Calendar.YEAR))
    array.add(calendar.get(Calendar.MONTH) + 1)
    array.add(calendar.get(Calendar.DAY_OF_MONTH))
    array.add(calendar.get(Calendar.HOUR))
    array.add(calendar.get(Calendar.MINUTE))
    array.add(calendar.get(Calendar.SECOND))

    array.toString
  }
  
  private def performViewQuery(view: View): (Seq[ArchiveEntry], Int) = db.view(view) match {
    case null => (Nil, 0)
    case v => (for(row <- v.getResults) yield new ArchiveEntry(row.getJSONObject), v.getInt("total_rows"))
  }
}

class ArchiveEntry(private val doc: JSONObject) {
  val updated = toDate(doc.getJSONArray("key"))
  val content = doc.getJSONObject("value").getString("Body")

  def toDate(array: JSONArray): java.util.Date = {
    val calendar = java.util.Calendar.getInstance()
    calendar.set(array.getInt(0), array.getInt(1) - 1, array.getInt(2), array.getInt(3), array.getInt(4), array.getInt(5))
    calendar.getTime
  }
}
