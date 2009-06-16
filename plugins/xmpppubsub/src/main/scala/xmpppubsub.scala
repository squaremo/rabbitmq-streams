/*
 * xmpppubsub.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

import net.sf.json.JSONObject
import net.lshift.feedshub.harness.Server
import org.jivesoftware.smackx.pubsub.PubSubManager
import org.jivesoftware.smack.{XMPPConnection,ConnectionConfiguration}

import net.lshift.feedshub.xmpppubsub.Dispatcher

class xmpppubsub(config : JSONObject) extends Server(config) {

    val conn = managerFromConfig(config)
    val dispatcher = new Dispatcher(log, conn)
    dispatcher.start

    private def managerFromConfig(config : JSONObject) : PubSubManager = {
        val service = config.getString("service")
        new PubSubManager(getConnectionFromConfig(config), service)
    }

    private def connnectionFromConfig(config : JSONObject) : XMPPConnection = {
        val options = new ConnectionConfiguration(config.getString("host"),
                                                  config.getInt("port"))
        // see SmackTestCase for more config we may want to fill out
        val conn = new XMPPConnection(options)
        conn.connect
        conn.login(config.getString("username"), config.getString("password"))
        conn
    }

}
