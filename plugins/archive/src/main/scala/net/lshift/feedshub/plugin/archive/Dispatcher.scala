/*
 * Dispatcher.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.lshift.feedshub.plugin.archive

import scala.actors.Actor
import scala.actors.Actor._

case class Entry(bytes : Array[Byte], key : String, ack : Unit => Unit)

class Dispatcher(config_url : String) extends Actor {
    private val destinations : Map[String, Destination] = Map()
    
    def act {
        loop {
            react {
                case entry@Entry(bytes, key, ack) =>
                    destinations.get(key) match {
                        case Some(destination) =>
                            destination ! entry
                        case None => true
                    }

            }
        }
    }
}