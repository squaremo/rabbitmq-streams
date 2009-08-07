package com.rabbitmq.streams.plugin.archive

import com.rabbitmq.streams.harness._
import com.fourspaces.couchdb.Document
import com.rabbitmq.client.QueueingConsumer.Delivery
import net.sf.json._
import com.rabbitmq.streams.plugin.archive._

import scala.collection.jcl.Conversions._

import com.fourspaces.couchdb._

class ArchiveServer() extends Server() {
  var dispatcher: Dispatcher = null

  override def configure(config: JSONObject) {
    if(config == null) throw new PluginBuildException("Unable to configure ArchiveServer from null configuration")
    
    val host = config.optString("host", "localhost")
    val port = config.optInt("port", 5984)
    val user = config.optString("username", "")
    val password = config.optString("password", "")
    
    val couch = new Session(host, port, user, password)

    dispatcher = new Dispatcher(log, couch)
    dispatcher.start
    super.configure(config)

    object input extends InputReader {
      override def handleMessage(msg: InputMessage) {
        log.debug("Input received: " + new String(msg.body))
        dispatcher ! Entry(msg.body, msg.routingKey, () => msg.ack())
      }
    }

    registerInput(input)
  }

  override def terminalStatusChange(destination: String, configs: java.util.List[JSONObject], active: Boolean) {
    dispatcher ! DestinationStatusChange(destination, List(configs: _*), active)
  }

}