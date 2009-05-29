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
import scala.collection.jcl.Conversions._

class websubscriber(config : JSONObject) extends Server(config) {

    //val couch = new Session("localhost", 5984, "", "") // TODO. Get from config.
    val dispatcher = new Dispatcher(log, (msg, id) => publishToDestination(msg.getBytes,  id))
    dispatcher.start

    override def terminalStatusChange(terminalId: String, configs : java.util.List[JSONObject], active : Boolean) {
        dispatcher ! StatusChange(terminalId, List(configs:_*), active)
    }

    postConstructorInit()
}
