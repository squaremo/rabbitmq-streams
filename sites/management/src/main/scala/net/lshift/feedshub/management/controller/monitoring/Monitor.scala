package net.lshift.feedshub.management.controller.monitoring

import scala.actors.Actor
import scala.actors.Actor._
import scala.collection.mutable.Map
import scala.collection.Set
import net.lshift.feedshub.management.controller.logging._
import net.lshift.feedshub.management.utils.actor.Stop

class FeedMonitor(feedSource: Actor with ObservableActor[UpdateFeedList], capacity: Int) extends Actor with ObservableActor[MonitorMessage] {

  private val monitors: Map[String, LogMonitor] = Map[String, LogMonitor]()

  def knownFeeds: Set[String] = monitors.keySet
  
  def lastLogMessage(feed: String): Option[LogMessage] = monitors.get(feed) match {
    case Some(monitor) => monitor.mostRecent
    case None          => None
  }
  
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
    monitors.get(component) match {
      case Some(monitor) => // monitor exists so leave it alone
      case None          => monitors.put(component, new LogMonitor(LogBinding.Any.withComponents(List(component)), capacity))
    }
    notifyObservers(MonitorMessage(component))
  }

  protected def newObserver(newbie: Actor) = newbie ! MonitoredFeeds(monitors.keySet)

  def act = {
    loop {
      react(handleCommands orElse handleObservers)
    }
  }

  connect
  start
}

object FeedMonitors extends FeedMonitor(Feeds, 100)

case class MonitorMessage(component: String)
case class MonitoredFeeds(feeds: Set[String])