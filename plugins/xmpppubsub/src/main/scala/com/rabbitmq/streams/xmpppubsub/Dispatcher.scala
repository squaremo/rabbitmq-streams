package com.rabbitmq.streams.xmpppubsub

/*
 * Dispatcher.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

import scala.actors.Actor
import scala.actors.Actor._

import com.fourspaces.couchdb.Document
import com.fourspaces.couchdb.Session
import com.rabbitmq.streams.harness._
import com.rabbitmq.client.QueueingConsumer.Delivery;

import net.sf.json.JSONObject

import org.jivesoftware.smack.{XMPPException}
import org.jivesoftware.smack.packet.XMPPError
import org.jivesoftware.smackx.pubsub.{PubSubManager,Node,Item}
import org.jivesoftware.smackx.pubsub.{SimplePayload,ConfigureForm,FormType,AccessModel}

import scala.collection.mutable.Map

case class Entry(bytes : Array[Byte], key : String, ack : (() => Unit))
case class DestinationStatusChange(destination: String, configs: List[JSONObject], active: Boolean)

class Payload(str : String) extends SimplePayload("text", "", (<text>{str}</text>).toString)

class Destination(endpoint : Node) {
  def publish(msg: Array[Byte]) {
    val payload = new Payload(new String(msg))
    val item = new Item[Payload](null, payload)
    endpoint.publish(item)
  }
}

class Dispatcher(log : Logger, conn : PubSubManager) extends Actor {
  private val destinationsMap : Map[String, List[Destination]] = Map()

  private def nodeFromConfig(config : JSONObject) : Node = {
    val nodeId = config.getString("node")
    try {
      try {
        conn.getNode(nodeId)
      }
      catch {
        case e : XMPPException if (e.getXMPPError.getCode == 404) => {
            log.error("Node " + nodeId + " not found.")
            throw e
            // TODO: Make this a configuration option, perhaps.
            // ...   In the meantime, it's safer to not arbitrarily create nodes.
            // ...   So: the uneccessary catch block remains.
            // val opts = new ConfigureForm(FormType.submit)
            // opts.setAccessModel(AccessModel.open)
            // conn.createNode(nodeId, opts)
            // conn.getNode(nodeId)
          }
      }
    }
    catch {
      case e => log.error(e); throw e
    }
  }

  def act() {
    loop {
      react {
        case Entry(bytes, key, ack) => {
            destinationsMap.get(key) match {
              case Some(destinations) => {
                  log.debug("Dispatch to " + key)
                  destinations.foreach(_.publish(bytes))
                  ack()
                }
              case None => {
                  log.debug("Not listening for: " + key)
                  ack()
                }
            }
          }
        case DestinationStatusChange(destination, configs, active) => {
            if (active) {
              log.info("Activating " + destination)
              if (! destinationsMap.contains(destination)) {
                val dests = configs.map(config => {
                    val node = nodeFromConfig(config.getJSONObject("destination"))
                    new Destination(node)
                  })
                destinationsMap += (destination -> dests)
                log.debug("Now listening to :" + destinationsMap.keySet toString)
              }
            }
            else {
              log.info("Deactivating " + destination)
              destinationsMap.get(destination) match {
                case Some(dests) => {
                    destinationsMap -= destination
                  }
                case None => log.warn("Cannot deactivate " + destination + "; not known or already inactive.")
              }
            }
          }
      }
    }
  }
}
