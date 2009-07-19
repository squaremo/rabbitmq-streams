/*
 * websubscriber.scala
 *
 */

import com.rabbitmq.streams.harness._
import com.fourspaces.couchdb.Document
import com.rabbitmq.client.QueueingConsumer.Delivery
import net.sf.json._
import com.rabbitmq.streams.plugin.websubscriber._

import com.fourspaces.couchdb._
import scala.collection.jcl.Conversions._

class websubscriber() extends Server() {

    val dispatcher = new Dispatcher(log, (msg, id) => publishToDestination(msg.getBytes,  id), privateDb)
    dispatcher.start

    override def terminalStatusChange(terminalId: String, configs : java.util.List[JSONObject], active : Boolean) {
        dispatcher ! StatusChange(terminalId, List(configs:_*), active)
    }

}
