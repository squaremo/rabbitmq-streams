package com.rabbitmq.streams.xmpppubsub

import net.sf.json.JSONObject
import com.rabbitmq.streams.harness.{Server, InputReader, InputMessage}
import com.rabbitmq.client.QueueingConsumer.Delivery
import org.jivesoftware.smack.proxy.ProxyInfo
import org.jivesoftware.smackx.pubsub.PubSubManager
import org.jivesoftware.smack.{XMPPConnection, ConnectionConfiguration}

import scala.collection.jcl.Conversions._

class XmppPubSubServer() extends Server() {

  var dispatcher : com.rabbitmq.streams.xmpppubsub.Dispatcher = null

  override def configure(config: JSONObject) {
    val conn = managerFromConfig(config)
    dispatcher = new Dispatcher(log, conn)
    dispatcher.start

    object input extends Server.ServerInputReader {
      override def handleBodyForTerminal(body : Array[Byte], terminalId : String, msg : InputMessage) {
        log.debug("Input received: " + new String(body))
        dispatcher ! Entry(body, terminalId, () => msg.ack())
      }
    }

    registerInput(input)
  }

  private def managerFromConfig(settings: JSONObject): PubSubManager = {
    val service = settings.getString("service")
    new PubSubManager(connectionFromConfig(settings), service)
  }

  private def connectionFromConfig(settings: JSONObject): XMPPConnection = {
    val options = configFromJson(settings)

    val conn = new XMPPConnection(options)
    conn.connect
      conn.login(settings.getString("username"),
                 settings.getString("password"),
                 settings.optString("resource", "feedshub-" + getId()))
      conn
    }

    private def configFromJson(settings: JSONObject): ConnectionConfiguration = {
      // see SmackTestCase for more config we may want to fill out
      val config = settings.containsKey("proxytype") match {
        case true => new ConnectionConfiguration(settings.getString("host"), settings.getInt("port"), proxyInfo(settings))
        case _ => new ConnectionConfiguration(settings.getString("host"), settings.getInt("port"))
      }
      config
    }

    private def proxyInfo(settings: JSONObject): ProxyInfo = settings.getString("proxytype") match {
      case "http" => ProxyInfo.forHttpProxy(settings.getString("proxyserver"), settings.getInt("proxyport"), settings.getString("proxyuser"), settings.getString("proxypassword"))
      case "socks4" => ProxyInfo.forHttpProxy(settings.getString("proxyserver"), settings.getInt("proxyport"), settings.getString("proxyuser"), settings.getString("proxypassword"))
      case "socks5" => ProxyInfo.forHttpProxy(settings.getString("proxyserver"), settings.getInt("proxyport"), settings.getString("proxyuser"), settings.getString("proxypassword"))
      case _ => ProxyInfo.forDefaultProxy
    }

  override def terminalStatusChange(destination: String, configs: java.util.List[JSONObject], active: Boolean) {
    dispatcher ! DestinationStatusChange(destination, List(configs: _*), active)
  }

}
