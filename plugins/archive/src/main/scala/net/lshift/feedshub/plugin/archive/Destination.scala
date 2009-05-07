/*
 * Destination.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.lshift.feedshub.plugin.archive

import scala.actors.Actor
import scala.actors.Actor._

case class NewEntry(bytes : Array[Byte], ack : Unit => Unit)

class Destination(postto: String) extends Actor {

    def act {
        loop {
            react {
                case NewEntry(bytes, ack) =>
                    val body = new String(bytes)
                    println(postto + " " + body)
                    ack()
            }
        }
    }
}
