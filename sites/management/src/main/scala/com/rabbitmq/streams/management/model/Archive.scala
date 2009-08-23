package com.rabbitmq.streams.management.model

import java.lang.String
import java.net.{URLEncoder, URL}
import java.text.ParseException
import java.util.{Calendar, Date}
import runtime.RichString
import scala.collection.jcl.Conversions._
import com.fourspaces.couchdb._
import net.sf.json._

class Server(url: String, configDatabaseName: String) {
  private val couch = Server.sessionFromString(url)
  private val configDb = couch.getDatabase(configDatabaseName)

  private val terminalConfigViewName = "_design/terminalconfig"
  private val terminalArchiveViewName = "_design/terminalarchive"

  createViewsIfNecessary

  private def createViewsIfNecessary {
    def createView(db: Database, name: String, viewName: String, scripts: (String, String)*) {
      db.saveDocument(constructView(name, viewName, scripts:_*), name)
    }

    def constructView(name: String, viewName: String, scripts: (String, String)*): Document =  {
      val document = new Document
      document.put("_id", "_design/" + name)
      document.put("language", "javascript")

      val functions = new JSONObject()
      for(script <- scripts) functions.put(script._1, script._2)

      val views = new JSONObject()
      views.put(viewName, functions)
      document.put("views", views)

      document
    }

    if(configDb.getDocument(terminalConfigViewName) == null) {
      createView(configDb, terminalConfigViewName, "byvalue",
        ("map", """
        function(doc) {
          if(doc.type == "terminal") {
            for(s in doc.servers) {
              keyvalues(doc.servers[s].source, doc._id);
              keyvalues(doc.servers[s].destination, doc._id);
            }
          }
        }

        function keyvalues(content, id)	{
          for(k in content)	{
            var array = [];
            array.push(content[k]);
            array.push(k);
            emit(content[k], id);
          }
        }
        """))
    }
    if(configDb.getDocument(terminalArchiveViewName) == null) {
      createView(configDb, terminalArchiveViewName, "byterminal",
        ("map", """
          function(doc)	{
           if(doc.type == "terminal")	{
            for(s in doc.servers) {
              emit(doc.servers[s].server, doc);
            }
           }
           if(doc.type == "server")	{
            emit(doc._id, doc);
           }
          }
        """),
        ("reduce", """
          function(key, values, rereduce) {
           if(rereduce) {
            return values;
           }
           var output = [];
           for(v in values) {
            if(values[v].type == "server" && values[v].server_type == "archive") {
             for(t in values) {
              if(values[t].type == "terminal") {
               for(s in values[t].servers) {
                if(values[t].servers[s].server == key[0][0]) {
                 output.push({terminal: values[t]._id, archive: key[0][0] + values[t].servers[s].destination.name});
                }
               }
              }
             }
            }
           }
           return output;
          }
        """))
    }
  }

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

  def byTerminal(terminal: String): Option[Archive] = {
    archives.dropWhile(a => a.terminalName != terminal).firstOption
  }

  def byName(name: String): Option[Archive] = {
    archives.dropWhile(a => a.name != name).firstOption
  }

  def terminals(query: String): Seq[Terminal] = {
    val view = new View("terminalconfig/byvalue")
    view.setStartKey(URLEncoder.encode(quoteIfNecessary(query), "UTF-8"))
    view.setEndKey(URLEncoder.encode(quoteIfNecessary(query), "UTF-8"))
    configDb.view(view) match {
      case null => Nil
      case v    => for(row <- v.getResults) yield new Terminal(row.getString("value"), byTerminal(row.getString("value")))
    }
  }

  def quoteIfNecessary(string:String): String = {
    try {
      string.toFloat
      string
    }
    catch  {
      case ex: NumberFormatException => "\"" + string + "\""
    }
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
    val entriesView = new View(entriesViewName)
    entriesView.setStartKey(startKey)
    entriesView.setEndKey(endKey)
    performViewQuery(entriesView)
  }

  def entries(startDate: Date, endDate: Date): (Seq[ArchiveEntry], Int) = {
    entries(toViewKey(startDate), toViewKey(endDate))
  }

  private def toViewKey(date: Date): String = {
    "" + date.getTime
  }

  private def performViewQuery(view: View): (Seq[ArchiveEntry], Int) = db.view(view) match {
    case null => (Nil, 0)
    case v => (for(row <- v.getResults) yield new ArchiveEntry(row.getJSONObject), v.getInt("total_rows"))
  }
}

class Terminal(val name: String, private val archive: Option[Archive]) {
  def archiveName = archive match {
    case Some(s) => s.name
    case None    => "No archive configured"
  }

  def archived:Boolean = archive match {
    case Some(s) => true
    case None    => false
  }
}

class ArchiveEntry(private val doc: JSONObject) {
  val updated = toDate(doc.getLong("key"))
  val content = doc.getJSONObject("value").getString("Body")

  def toDate(value: Long): java.util.Date = {
    val calendar = java.util.Calendar.getInstance()
    calendar.setTimeInMillis(value)
    calendar.getTime
  }
}
