package net.lshift.feedshub.management.controller

import collection.mutable.Queue
import scala.actors.Actor
import scala.actors.Actor._

case object Stop

class LogMonitor(matches: Function1[Array[String], Boolean]) extends Actor {
  val buffer: Queue[LogMessage] = new Queue()

  def act() = {
    loop{
      react{
        case msg@LogMessage(level, desc, component) if (matches(component)) => buffer += msg
        case Stop => exit("stop")
      }
    }
  }

  start
}

object LogMonitor {
  def AllComponents: Function1[Array[String], Boolean] = _ => true
}

