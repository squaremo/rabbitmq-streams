/*
 * Dispatcher.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.lshift.feedshub.plugin.archive

import scala.actors.Actor
import scala.actors.Actor._

import com.fourspaces.couchdb.Document
import com.fourspaces.couchdb.Session
import net.lshift.feedshub.harness._
import com.rabbitmq.client.QueueingConsumer.Delivery;

import scala.collection.mutable.Map

case class Entry(bytes : Array[Byte], key : String, ack : (() => Unit))
case class DestinationStatusChange(destination: String, ack : (() => Unit))

class Dispatcher(log : Logger, terminalConfig : String => Document, terminalStatus : String => Document, couch : Session) extends Actor {
    private val destinations : Map[String, Destination] = Map()

    def act {
        loop {
            react {
                case Entry(bytes, key, ack) => {
                        destinations.get(key) match {
                            case Some(destination) => {
                                log.debug("Dispatch to " + key)
                                destination ! NewEntry(bytes, ack)
                            }
                            case None => {
                                log.debug("Not listening for: " + key)
                                ack()
                            }
                        }
                    }
                case DestinationStatusChange(destination, ack) => {
                        val status = terminalStatus(destination)
                        if (status.getBoolean("active")) {
                            log.debug("Activating " + destination)
                            if (! destinations.contains(destination)) {
                                val dbname = "archive_" + destination
                                couch.createDatabase(dbname)
                                val db = couch.getDatabase(dbname)
                                val d = new Destination(log, db)
                                destinations.incl(destination -> d)
                                log.debug("Now listening to :" + destinations.toString)
                                d.start
                            }
                        }
                        else {
                            log.debug("Deactivating " + destination)
                            destinations.excl(destination)
                        }
                        ack()
                    }
            }
        }
    }
}
