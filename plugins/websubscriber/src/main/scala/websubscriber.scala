/*
 * websubscriber.scala
 *
 */

import net.lshift.feedshub.harness._
import com.fourspaces.couchdb.Document
import com.rabbitmq.client.QueueingConsumer.Delivery
import net.sf.json._
import net.lshift.feedshub.plugin.websubscriber._

import com.fourspaces.couchdb._

class websubscriber(config : JSONObject) extends Server(config) {

    val couch = new Session("localhost", 5984, "", "") // TODO. Get from config.
    val dispatcher = new Dispatcher(log, this.terminalConfig, this.terminalStatus, couch)
    dispatcher.start

    object command extends InputReader {
        def handleDelivery(pkg : Delivery) {
            new String(pkg.getBody, "US-ASCII") match {
                case "status change" => {
                    val serverAndTerminal = pkg.getEnvelope.getRoutingKey
                    log.debug("Status change: " + serverAndTerminal)
                    serverAndTerminal.split("\\.") match {
                        case Array(server, terminal) =>
                            dispatcher ! StatusChange(terminal, () => ack(pkg))
                        case _ =>
                            log.warn("Malformed status message to topic " + serverAndTerminal)
                    }
                }
            }
            ack(pkg)
        }
    }

    postConstructorInit()
}
