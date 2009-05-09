/*
 * Destination.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.lshift.feedshub.plugin.archive

import scala.actors.Actor
import scala.actors.Actor._

import java.util.Date

import net.lshift.feedshub.harness.Logger
import com.fourspaces.couchdb._

case class NewEntry(bytes : Array[Byte], ack : () => Unit)

class Destination(log : Logger, postto: Database) extends Actor {

    def act {
        loop {
            react {
                case NewEntry(bytes, ack) =>
                    val body = new String(bytes)
                    log.debug("Message received at " + postto + " of " + body)
                    val doc = new Document
                    doc.put("updated", new Date().getTime)
                    doc.put("body", body)
                    postto.saveDocument(doc)
                    ack()
            }
        }
    }
}
