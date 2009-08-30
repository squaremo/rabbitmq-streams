package com.rabbitmq.streams.plugin.archive

import scala.actors.Actor
import scala.actors.Actor._

import com.fourspaces.couchdb.Document
import com.fourspaces.couchdb.Session
import com.rabbitmq.streams.harness._
import com.rabbitmq.client.QueueingConsumer.Delivery;

import net.sf.json.JSONObject

import scala.collection.mutable.Map

case class Entry(bytes : Array[Byte], key : String, ack : (() => Unit))
case class DestinationStatusChange(destination: String, configs: List[JSONObject], active: Boolean)

class Dispatcher(log : Logger, couch : Session) extends Actor {
  private val destinationsMap : Map[String, List[Destination]] = Map()
  private val viewName = "_design/by_date"

  def act() {
    loop {
      react {
        case Entry(bytes, key, ack) => {
            destinationsMap.get(key) match {
              case Some(destinations) => {
                log.debug("Dispatch to " + key)
                destinations.foreach(_ ! NewEntry(bytes))
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
                  val destConfig = config.getJSONObject("destination")
                  val dbname = "archive_" + destination + destConfig.getString("name")
                  couch.createDatabase(dbname)
                  val db = couch.getDatabase(dbname)
                  if(db.getDocument(viewName) == null) {
                    val view = new Document()
                    view.setId(viewName)
                    view.put("language", "javascript")

                    val mapFunction = """
                      function(doc) {
                        emit(doc.updated, {Body: doc.body})
                      }
                    """
                    view.addView(viewName, "by_date", mapFunction)
                    db.saveDocument(view, viewName)
                  }
                  new Destination(log, db)
                })
              destinationsMap += (destination -> dests)
              dests.foreach(_.start)
              log.debug("Now listening to :" + destinationsMap.keySet toString)
            }
          }
          else {
            log.info("Deactivating " + destination)
            destinationsMap.get(destination) match {
              case Some(dests) => {
                  dests.foreach(_.exit)
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
