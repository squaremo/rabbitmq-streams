/*
 * Destination.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.lshift.feedshub.plugin.archive

import scala.actors.Actor
import scala.actors.Actor._

class Destination(postto: String) extends Actor {

    def act {
        loop {
            react {
                case Entry(bytes, key, ack) =>
                    val body = new String(bytes)
                    println(postto + " " + key + " " + body)
                    ack()
            }
        }
    }
}
