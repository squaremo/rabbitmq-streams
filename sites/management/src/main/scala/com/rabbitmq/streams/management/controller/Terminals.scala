package com.rabbitmq.streams.management.controller

import scala.collection.jcl.Conversions._
import scala.actors.Actor
import scala.actors.Actor._

import com.fourspaces.couchdb._
import net.sf.json.JSONObject

case class TerminalStatus(id: String, active: Boolean, source: Boolean, destination: Boolean, server: String)

case class UpdateTerminalsList(terminals: List[TerminalStatus])

object Terminals extends Actor with FeedsHubConfig with ConfigAwareActor with ObservableActor[UpdateTerminalsList] {
  val TerminalStatusView = "terminals/join?group=true"

  override def bindingKey: String = "*.*" // i.e., only things with two components

  var terminalstata: List[TerminalStatus] = Nil

  private def terminalStatusFromJSON(id: String, json: JSONObject): TerminalStatus = {
    val status = json.getJSONObject("terminal-status")
    val config = json.getJSONObject("terminal")
    val source = config.has("source")
    val destination = config.has("destination")
    val servers = config.getJSONArray("servers")
    val firstServer = servers.getJSONObject(0)
    val server = firstServer.get("server").toString
    TerminalStatus(id, status.getBoolean("active"), source, destination, server)
  }

  def readStatus {
    val vr = statusDb.view(TerminalStatusView).getResults
    terminalstata = vr.map(v => terminalStatusFromJSON(v.getString("key"), v.getJSONObject("value"))).toList
  }

  def newObserver(newbie: Actor) {
    newbie ! UpdateTerminalsList(terminalstata)
  }

  def notifyOfUpdate {
    notifyObservers(UpdateTerminalsList(terminalstata))
  }

  val handleChanges: PartialFunction[Any, Unit] = {
    case ConfigChange(id) => true // ignore for now
    case StatusChange(id) => readStatus; notifyOfUpdate
  }

  def act {
    loop {
      react(handleChanges orElse handleObservers)
    }
  }

  readStatus
  start
}
