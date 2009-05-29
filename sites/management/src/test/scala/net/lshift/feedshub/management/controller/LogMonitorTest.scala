package net.lshift.feedshub.management.controller


import scala.actors.Actor
import scala.actors.Actor._
import org.scalatest.junit.JUnit3Suite

class LogMonitorTest extends JUnit3Suite  {

  def testFilteringOfMessages = {
    val monitor = new LogMonitor(LogMonitor.AllComponents) {
      override def scheduler = new scala.actors.SingleThreadedScheduler
    }

    monitor ! LogMessage(Error, "Bang!", Array("X", "Y", "Z"))
    expect(1) {monitor.buffer.size}

    monitor ! Stop
  }
}