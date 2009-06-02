/*
 * Archive.scala
 *
 */

package net.lshift.feedshub.feedserver.model

import java.net.URL
import scala.collection.jcl.Conversions._

import com.fourspaces.couchdb._
import net.sf.json._

/**
 * Represents a (CouchDB) database server
 */
class Server(url : String, configDatabaseName : String) {
    val couch = sessionFromString(url)
    val configDb = couch.getDatabase(configDatabaseName)

    private def sessionFromString(urlStr : String) : Session = {
        val url = new URL(urlStr)
        new Session(url.getHost(), url.getPort())
    }

    private def newArchive(server : JSONObject, terminal: JSONObject) : Archive = {
        new Archive("http://localhost:5984/archive_"+terminal.get("_id"), terminal)
    }

    def archives : Seq[Archive] = {
        for (row <- configDb.view("terminals/byserver?group=true").getResults;
             server = row.getJSONObject("value")
             if server.getJSONObject("server").getString("server_type").equals("archive");
             t <- server.getJSONArray("terminals");
             terminal = t.asInstanceOf[JSONObject];
             destination = terminal.getJSONObject("server").getJSONObject("destination");
             if destination.optBoolean("publish", false))
        yield newArchive(server.getJSONObject("server"), destination)
    }

    def archive(name : String) : Option[Archive] = {
        archives.dropWhile(a => a.name != name).firstOption
    }
}

object LocalServer extends Server("http://localhost:5984", "feedshub_status")

/**
 * Represents an archive available as a feed
 */
class Archive(val dbUrl : String, config : JSONObject) {
    val name = config.getString("name")
    val title = config.getString("title")
}
