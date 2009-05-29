/*
 * archive.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

import net.lshift.feedshub.harness._
import com.fourspaces.couchdb.Document
import com.rabbitmq.client.QueueingConsumer.Delivery
import net.sf.json._
import net.lshift.feedshub.plugin.archive._

import scala.collection.jcl.Conversions._

import com.fourspaces.couchdb._

class archive(config : JSONObject) extends Server(config) {

    val couch = new Session("localhost", 5984, "", "") // TODO. Get from config.
    val dispatcher = new Dispatcher(log, couch)
    dispatcher.start

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
