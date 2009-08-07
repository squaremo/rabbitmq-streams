package com.rabbitmq.streams.plugin.archive

import scala.actors.Actor
import scala.actors.Actor._

import java.util.Date

import com.rabbitmq.streams.harness.Logger
import com.fourspaces.couchdb._

case class NewEntry(bytes : Array[Byte])

class Destination(log : Logger, postto: Database) extends Actor {

  def act {
    loop {
      react {
        case NewEntry(bytes) =>
          val body = new String(bytes)
          log.debug("Message received at " + postto + " of " + body)
          val doc = new Document
          doc.put("updated", new Date().getTime)
          doc.put("body", body)
          log.debug("Saving doc" + doc.toString)
          postto.saveDocument(doc)
      }
    }
  }
}
