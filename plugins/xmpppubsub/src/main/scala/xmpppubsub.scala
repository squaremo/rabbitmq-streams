/*
 * xmpppubsub.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

import net.sf.json.JSONObject
import net.lshift.feedshub.harness.{Server,InputReader}
import com.rabbitmq.client.QueueingConsumer.Delivery
import org.jivesoftware.smackx.pubsub.PubSubManager
import org.jivesoftware.smack.{XMPPConnection, ConnectionConfiguration}

import scala.collection.jcl.Conversions._

import net.lshift.feedshub.xmpppubsub.{Dispatcher,Entry,DestinationStatusChange}

class xmpppubsub(config : JSONObject) extends Server(config) {

    val settings = config.getJSONObject("configuration")
    log.debug(settings.toString)
    val conn = managerFromConfig(settings)
    val dispatcher = new Dispatcher(log, conn)
    dispatcher.start

    private def managerFromConfig(settings : JSONObject) : PubSubManager = {
        val service = settings.getString("service")
        new PubSubManager(connectionFromConfig(settings), service)
    }

    private def connectionFromConfig(settings : JSONObject) : XMPPConnection = {
        val options = new ConnectionConfiguration(settings.getString("host"),
                                                  settings.getInt("port"))
        // see SmackTestCase for more config we may want to fill out
        val conn = new XMPPConnection(options)
        conn.connect
        conn.login(settings.getString("username"), settings.getString("password"))
        conn
    }

    object input extends InputReader {
        override def handleDelivery(pkg : Delivery) {
            log.debug("Input received: " + new String(pkg.getBody))
            dispatcher ! Entry(pkg.getBody, pkg.getEnvelope.getRoutingKey.toString, () => ack(pkg))
        }
    }

    override def terminalStatusChange(destination : String, configs : java.util.List[JSONObject], active : Boolean) {
        dispatcher ! DestinationStatusChange(destination, List(configs:_*), active)
    }

    postConstructorInit()


}
