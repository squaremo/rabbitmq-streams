/*
 * archive.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

import net.lshift.feedshub.harness._
import com.rabbitmq.client.QueueingConsumer.Delivery
import net.sf.json._
import net.lshift.feedshub.plugin.archive._

class archive(config : JSONObject) extends Server(config) {

    private val terminalsUrl = config.getString("terminals_database") // TODO move into harness.Server

    val dispatcher = new Dispatcher()

    class InputHandler extends InputReader {
        override def handleDelivery(pkg : Delivery) {
            dispatcher ! Entry(pkg.getBody, pkg.getEnvelope.getRoutingKey, () => ack(pkg))
        }
    }

    class CommandHandler extends InputReader {
        def handleDelivery(pkg : Delivery) {
            new String(pkg.getBody, "US-ASCII") match {
                case "status change" =>
                    val serverAndDestination = pkg.getEnvelope.getRoutingKey
                    log.debug("Status change: " + serverAndDestination)
                    ack(pkg)
            }
        }
    }

    val input = new InputHandler()
    val command = new CommandHandler()

    postConstructorInit()
}
