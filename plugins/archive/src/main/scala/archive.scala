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

import com.fourspaces.couchdb._

class archive(config : JSONObject) extends Server(config) {

    val couch = new Session("localhost", 5984) // TODO. Get from config.
    val dispatcher = new Dispatcher(log, this.terminalConfig, this.terminalStatus, couch)

    object input extends InputReader {
        override def handleDelivery(pkg : Delivery) {
            log.debug("Input received: " + new String(pkg.getBody))
            dispatcher ! Entry(pkg.getBody, pkg.getEnvelope.getRoutingKey, () => ack(pkg))
        }
    }

    object command extends InputReader {
        def handleDelivery(pkg : Delivery) {
            new String(pkg.getBody, "US-ASCII") match {
                case "status change" =>
                    val serverAndDestination = pkg.getEnvelope.getRoutingKey
                    log.debug("Status change: " + serverAndDestination)
                    serverAndDestination.split("\\.") match {
                        case Array(server, destination) =>
                            dispatcher ! DestinationStatusChange(destination)
                        case _ =>
                            log.warn("Malformed status message to topic " + serverAndDestination)
                    }
                    ack(pkg)
            }
        }
    }

    postConstructorInit()
}
