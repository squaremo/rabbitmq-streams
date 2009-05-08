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

case class Entry(bytes : Array[Byte], key : String, ack : (() => Unit))
case class DestinationStatusChange(destination: String)

class Dispatcher(log : Logger, terminalConfig : String => Document, terminalStatus : String => Document, couch : Session) extends Actor {
    private val destinations : Map[String, Destination] = Map()

    def act {
        loop {
            react {
                case Entry(bytes, key, ack) => {
                        destinations.get(key) match {
                            case Some(destination) =>
                                destination ! NewEntry(bytes, ack)
                            case None => true
                        }
                    }
                case DestinationStatusChange(destination) => {
                        val status = terminalStatus(destination)
                        if (status.getBoolean("active"))
                            if (destinations.contains(destination)) {
                                val db = couch.createDatabase(destination)
                                destinations.incl(destination -> new Destination(log, db))
                            }
                        else destinations.excl(destination)
                    }
            }
        }
    }
}
