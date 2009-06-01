package net.lshift.feedshub.management.controller

import collection.mutable.Queue
import scala.actors.Actor
import scala.actors.Actor._

class LogMonitor(binding: LogBinding, bufferSize: Int) extends Logger(binding) {
  val messages = new RollingQueue {
    type T = LogMessage
    val maximumSize = bufferSize
  }

  def processMessage(message: LogMessage) = messages.enqueue(message)
}
