package com.rabbitmq.streams.management.controller.logging

import org.scalatest.junit.JUnit3Suite

class RollingQueueTest extends JUnit3Suite {
  def testConcreteQueue = {
    val q = new RollingQueue {
      type T = String
      val maximumSize = 2
    }
    expect(None) {q.dequeue}

    expect(Some("BANG")) {
      q.enqueue("BANG")
      q.dequeue
    }

    expect(Some("3")) {
      q.enqueue("1")
      q.enqueue("2")
      q.enqueue("3")
      q.enqueue("4")

      q.dequeue
    }

    expect(Some("4")) {q.dequeue}
    expect(None) {q.dequeue}
  }

  def testNoAccessToUnderlyingQueue = {
    val q = new RollingQueue {
      type T = String
      val maximumSize = 1
    }

    expect(None)  {
      val q1 = q.contents
      q1.enqueue("BANG")
      q1.enqueue("BANG")
      q.dequeue
    }
  }
}
