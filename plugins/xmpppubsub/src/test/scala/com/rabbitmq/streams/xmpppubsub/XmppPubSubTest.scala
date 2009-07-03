package com.rabbitmq.streams.xmpppubsub

import org.specs._
import org.specs.runner.JUnit

class XmppPubSubTest extends Specification with JUnit {
  "x" should {
    "1 character in x" in {
      "x".size must_== 1
    }
  }
}