/*
 * Terminals.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.lshift.feedshub.management.controller

import scala.collection.jcl.Conversions._
import scala.actors.Actor
import scala.actors.Actor._

case class TerminalStatus(name : String, active: Boolean)

case class UpdateTerminalsList(terminals : List[TerminalStatus])

object Terminals extends Actor with FeedsHubConfig with ConfigAwareActor with ObservableActor[UpdateTerminalsList] {
    val TerminalStatusView = "terminals/status"
    
    override def bindingKey : String = "*.*" // i.e., only things with two components

    private var terminalstata : List[TerminalStatus] = Nil

    def readStatus {
        val vr = statusDb.queryView(TerminalStatusView, classOf[Boolean], null, null).getRows
        terminalstata = vr.map(v => TerminalStatus(v.getKey.toString, v.getValue)) toList
    }

    def newObserver(newbie : Actor) {
        newbie ! UpdateTerminalsList(terminalstata)
    }

    def notifyOfUpdate {
        notifyObservers(UpdateTerminalsList(terminalstata))
    }

    val handleChanges : PartialFunction[Any, Unit] = {
        case ConfigChange(id) =>
            true // ignore for now
        case StatusChange(id) =>
            readStatus; notifyOfUpdate
    }

    def act {
        loop {
            react (handleChanges orElse handleObservers)
        }
    }

    readStatus
    start
}
