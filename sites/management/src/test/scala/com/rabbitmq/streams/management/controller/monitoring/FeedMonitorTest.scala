package com.rabbitmq.streams.management.controller.monitoring

import bootstrap.liftweb.{RabbitConfig, CouchConfig, Boot}
import scala.actors.Actor
import scala.actors.Actor._
import org.scalatest.junit.JUnit3Suite
import com.rabbitmq.streams.management.utils.actor.Stop

class FeedMonitorTest extends JUnit3Suite {
  object testCouch extends CouchConfig {
    override def server = "server"
  }
  Boot.couchConfig(testCouch)

  object testRabbit extends RabbitConfig  {
    override def server = "localhost"
    override def port = 5672
    override def username = "feedshub_admin"
    override def password = "feedshub_admin"
  }
  Boot.rabbitConfig(testRabbit)

  val feedName = "feed1"
  val feedStatus = FeedStatus(feedName, false)
  val feedSource = new Actor with ObservableActor[UpdateFeedList] {
    override def scheduler = new scala.actors.SingleThreadedScheduler

    protected def newObserver(newbie: Actor) = {
      newbie ! UpdateFeedList(List(feedStatus))
    }

    val handleCommands: PartialFunction[Any, Unit] = {
      case Stop => exit("stop")
    }

    def act() = {
      loop(react(handleCommands orElse handleObservers))
    }
    start
  }

  def testMonitorCreation {
    val monitor = new FeedMonitor(feedSource, 10) {
      override def scheduler = new scala.actors.SingleThreadedScheduler
    }

    expect(Set(feedName)) {monitor.knownFeeds}

    monitor ! Stop
    feedSource ! Stop
  }
}
