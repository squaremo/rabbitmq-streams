package net.lshift.feedshub.management.controller.monitoring

import scala.actors.Actor
import scala.actors.Actor._
import scala.collection.mutable.Map
import net.lshift.feedshub.management.controller.logging._
import net.lshift.feedshub.management.utils.actor.Stop

class FeedMonitor(feedSource: Actor with ObservableActor[UpdateFeedList], capacity: Int) extends Actor with ObservableActor[MonitorMessage] {

  val monitors: Map[String, LogMonitor] = Map[String, LogMonitor]()

  def connect = {
    feedSource ! Observe(this)
  }

  val handleCommands: PartialFunction[Any, Unit] = {
    case UpdateFeedList(list) =>
      list.foreach(s => updateMonitor(s.id))
    case Stop =>
      feedSource ! Unobserve(this)
      exit(this)
  }

  private def updateMonitor(component: String) = {
    monitors.update (component, new LogMonitor(LogBinding.Any.withComponents(List(component)), capacity))
    notifyObservers(MonitorMessage(component))
  }

  protected def newObserver(newbie: Actor) = null

  def act = {
    loop {
      react(handleCommands orElse handleObservers)
    }
  }

  connect
  start
}

case class MonitorMessage(component: String)