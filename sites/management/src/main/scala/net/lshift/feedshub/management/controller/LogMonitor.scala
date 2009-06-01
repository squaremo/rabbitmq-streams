package net.lshift.feedshub.management.controller

import collection.mutable.HashSet
import collection.mutable.Queue
import scala.actors.Actor
import scala.actors.Actor._

class LogMonitor(binding: LogBinding, capacity: Int) extends Logger(binding) {
  private val messages = new RollingQueue {
    type T = LogMessage
    val maximumSize = capacity

    def history(level: LogLevel): List[LogMessage] = {
      val levels = level.andUp
      contents.filter(msg => levels.contains(msg.level)).toList
    }
  }

  def processMessage(message: LogMessage) = messages.enqueue(message)

  def history(level: LogLevel) = messages.history(level)

  def size = messages.size

  val listeners = Map(
    Debug -> HashSet[Actor](),
    Info -> HashSet[Actor](),
    Warn -> HashSet[Actor](),
    Error -> HashSet[Actor](),
    Fatal -> HashSet[Actor]())

  override def handlers: PartialFunction[Any, Unit] = {
    val localHandlers: PartialFunction[Any, Unit] = {
      case AddLogListener(level, listener) =>
        listener ! History(history(level))
        listeners(level) += listener
      case RemoveLogListener(listener) =>
        listeners.values.foreach(level => level -= listener)
    }
    localHandlers orElse super.handlers
  }

  def notifyListeners(message: LogMessage) {
    for(level <- message.level.andDown) {
      listeners(level).foreach(listener => listener ! message)
    }
  }
}
