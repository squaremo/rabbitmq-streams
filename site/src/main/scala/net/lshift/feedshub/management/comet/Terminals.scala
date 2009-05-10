/*
 * Terminals.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.lshift.feedshub.management.comet

import net.lshift.feedshub.management.controller._

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
                       <tr><th>Terminal</th><th>Status</th><th></th></tr>
                </thead>
                <tbody>
                       {terminals.map(t =>
                  <tr>
                      <td>{t.name}</td>
                      <td class="status">{if (t.active) "Active" else "Inactive"}</td>
                      <td class="ctrl"></td>
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
        Terminals ! Observe(this)
    }

    override def lowPriority : PartialFunction[Any, Unit] = {
        case UpdateTerminalsList(newList) => terminals = newList; reRender(false)
    }
}
