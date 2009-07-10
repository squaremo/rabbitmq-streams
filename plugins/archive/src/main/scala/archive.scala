/*
 * archive.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

import com.rabbitmq.streams.harness._
import com.fourspaces.couchdb.Document
import com.rabbitmq.client.QueueingConsumer.Delivery
import net.sf.json._
import com.rabbitmq.streams.plugin.archive._

import scala.collection.jcl.Conversions._

import com.fourspaces.couchdb._

class archive(config : JSONObject) extends Server(config) {

    val couch = new Session("localhost", 5984, "", "") // TODO. Get from config.
    val dispatcher = new Dispatcher(log, couch)
    dispatcher.start

    object input extends Server.ServerInputReader {
        override def handleBodyForTerminal(body : Array[Byte], key : String, tag : Long) {
            log.debug("Input received: " + new String(body))
            dispatcher ! Entry(body, key, () => ack(tag))
        }
    }

    registerHandler("input", input)


    override def terminalStatusChange(destination : String, configs : java.util.List[JSONObject], active : Boolean) {
        dispatcher ! DestinationStatusChange(destination, List(configs:_*), active)
    }

}
