/*
 * Archive.scala
 *
 */

package com.rabbitmq.streams.feedserver.model

import java.net.URL
import scala.collection.jcl.Conversions._

import com.fourspaces.couchdb._
import net.sf.json._

/**
 * Represents a (CouchDB) database server
 */
class Server(url : String, configDatabaseName : String) {
    val couch = Server.sessionFromString(url)
    val configDb = couch.getDatabase(configDatabaseName)

    private def newArchive(server : JSONObject, terminal: JSONObject, destination : JSONObject) : Archive = {
        new Archive(couch, "archive_"+terminal.get("_id")+destination.get("name"), destination)
    }

    def archives : Seq[Archive] = {
        for (row <- configDb.view("terminals/byserver?group=true").getResults;
             server = row.getJSONObject("value")
             if server.getJSONObject("server").getString("server_type").equals("archive");
             t <- server.getJSONArray("terminals");
             terminal = t.asInstanceOf[JSONObject];
             destination = terminal.getJSONObject("server").getJSONObject("destination");
             if destination.optBoolean("publish", false))
        yield newArchive(server.getJSONObject("server"), terminal, destination)
    }

    def archive(name : String) : Option[Archive] = {
        archives.dropWhile(a => a.name != name).firstOption
    }
}
object Server {
    def sessionFromString(urlStr : String) : Session = {
        val url = new URL(urlStr)
        new Session(url.getHost(), url.getPort())
    }
}

object LocalServer extends Server("http://localhost:5984", "feedshub_status")

/**
 * Represents an archive available as a feed
 */
class Archive(private val couch: Session, private val dbName : String, private val config : JSONObject) {
    val name = config.getString("name")
    val title = config.getString("title")

    private val db = couch.getDatabase(dbName)

    def installView {
        val d = new Document()
        d.addView("entries", "updated", "function(doc) {emit(doc.updated, doc);}")
        db.saveDocument(d)
    }

    def entries(limit : Int) : Seq[ArchiveEntry] = {
        val entriesView = new View("entries/updated")
        entriesView.setLimit(limit); entriesView.setDescending(true);
        db.view(entriesView) match {
            case null => installView; entries(limit)
            case view =>
                for (row <- view.getResults;
                     doc = row.getJSONObject("value"))
                yield new ArchiveEntry(doc)
        }
    }
}

class ArchiveEntry(private val doc : JSONObject) {
    val updated = doc.getLong("updated")
    val content = doc.getString("body")
}
