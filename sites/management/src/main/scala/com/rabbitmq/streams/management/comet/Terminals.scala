/*
 * Terminals.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package com.rabbitmq.streams.management.comet

import com.rabbitmq.streams.management.controller._

import scala.collection.mutable.HashMap
import scala.xml.{NodeSeq, Text}

import net.liftweb.http._
import net.liftweb.http.js.JsCmds.Noop
import net.liftweb.util.{Box, Full}

import net.liftweb.widgets.tablesorter.TableSorter

class Terminals extends CometActor {
    var terminals : List[TerminalStatus] = Nil

    override def defaultPrefix = Full("t")

    override def render : RenderOut = {
        bind("list" ->
             (<table id="terminals-list" class="tablesorter status-list">
                <thead>
                       <tr><th>Terminal</th><th>Status</th><th>Source?</th><th>Destination?</th><th>Server</th></tr>
                </thead>
                <tbody>
                       {terminals.map(t =>
                  <tr>
                      <td>{t.id}</td>
                      <td class="status">{if (t.active) "Active" else "Inactive"}</td>
                      <td>{if (t.source) "Yes" else "No"}</td>
                      <td>{if (t.destination) "Yes" else "No"}</td>
                      <td>{t.server}</td>
                  </tr>)}
                </tbody>
              </table>
              <script>
                {TableSorter.jsRender("terminals-list")}
              </script>))
    }

    override def localSetup {
        Terminals ! Observe(this)
    }

    override def localShutdown {
        Terminals ! Unobserve(this)
    }

    override def lowPriority : PartialFunction[Any, Unit] = {
        case UpdateTerminalsList(newList) => terminals = newList; reRender(false)
    }
}
