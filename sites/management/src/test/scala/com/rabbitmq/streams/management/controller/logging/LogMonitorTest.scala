package com.rabbitmq.streams.management.controller.logging

import scala.actors.Actor
import scala.actors.Actor._
import org.scalatest.junit.JUnit3Suite
import com.rabbitmq.streams.management.utils.actor.Stop

class LogMonitorTest extends JUnit3Suite {
  def testFilteringOfMessages = {
    val monitor = new LogMonitor(LogBinding.Any, 100) {
      override def scheduler = new scala.actors.SingleThreadedScheduler // Override for unit testing

      override def connect {} // Override so that unit test doesn't need an active exchange
    }

    monitor ! LogMessage(Error, "Bang!", Array("X", "Y", "Z"))
    expect(1) {monitor.size}

    monitor ! Stop
  }
}
