/*
 * Destination.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.lshift.feedshub.plugin.archive

import scala.actors.Actor
import scala.actors.Actor._

import net.lshift.feedshub.harness.Logger

case class NewEntry(bytes : Array[Byte], ack : () => Unit)

class Destination(postto: String, log : Logger) extends Actor {

    def act {
        loop {
            react {
                case NewEntry(bytes, ack) =>
                    val body = new String(bytes)
                    ack()
            }
        }
    }
}
