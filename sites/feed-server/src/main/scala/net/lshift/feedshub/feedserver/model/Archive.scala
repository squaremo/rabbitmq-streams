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
        for (server <- configDb.view("terminals/byserver?group=true").getResults;
            if server.getJSONObject("server").getString("server_type").equals("archive");
            t <- server.getJSONArray("terminals");
            terminal = t.asInstanceOf[JSONObject];
            if terminal.optBoolean("publish", false))
       yield newArchive(server.getJSONObject("server"), terminal)
    }

}

/**
 * Represents an archive available as a feed
 */
class Archive(val dbUrl : String, config : JSONObject) {
    val name = config.getString("_id") // for now
}