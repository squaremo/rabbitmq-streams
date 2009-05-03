/*
 * LogActor.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.lshift.feedshub.management.comet

import scala.xml.{NodeSeq, Text}

import net.liftweb.http._
import net.liftweb.http.js.JsCmds.Noop
import net.liftweb.util.{Box, Full}

import net.lshift.feedshub.management.controller._

class LogActor extends CometActor {

    override def defaultPrefix = Full("log")
    
    var level : LogLevel = Warn
    var messages : List[LogMessage] = List()

    def changeLevel(newLevel: LogLevel) {
        level = newLevel
        println(newLevel)
        Log ! RemoveLogListener(this)
        Log ! AddLogListener(level, this)
    }

    def controls : NodeSeq = {
        LogLevel.values.map(
            level => SHtml.ajaxButton(level.stringValue, () => {changeLevel(level); Noop})
        )
    }

    def list : NodeSeq = {
        <ol>
            {messages.map(msg => <li>{msg.level.stringValue} {msg.msg}</li>)}
        </ol>
    }

    override def lowPriority : PartialFunction[Any, Unit] = {
        case History(ms) => println("History"); messages = ms; reRender(false)
        case msg@LogMessage(a1, a2) => println("LogMessage " + a2); messages = msg :: messages; reRender(false)
    }

    override def localSetup {
        Log ! AddLogListener(level, this)
    }

    override def localShutdown {
        Log ! RemoveLogListener(this)
    }

    override def render : RenderOut = {
        println("Binding list and controls")
        bind("controls" -> controls,
             "list" -> list)
    }
}
