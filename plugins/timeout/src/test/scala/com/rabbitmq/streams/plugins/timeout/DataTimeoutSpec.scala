package com.rabbitmq.streams.plugins.timeout

import net.sf.json._
import com.rabbitmq.streams.harness.{MessageChannel, InputHandler}

import org.specs._
import org.specs.mock.Mockito
import org.mockito.Matchers._
import org.mockito.Matchers
import org.specs.runner.{ConsoleRunner, JUnit4}
import com.rabbitmq.streams.harness.testsupport.MockMessageChannel
import com.rabbitmq.streams.harness.InputMessage

class DataTimeoutTest extends JUnit4(DataTimeoutSpec)
//class MySpecSuite extends ScalaTestSuite(DataTimeoutSpec)
object MySpecRunner extends ConsoleRunner(DataTimeoutSpec)


object DataTimeoutSpec extends Specification with Mockito {

  val config = new JSONObject
  config.put("timeout", 3)
  config.put("message", "MESSAGE GOES HERE")

  "Data timeout " should {
    "be nullary-constructable" in {
      val dt = new DataTimeout
      dt must notBeNull
    }

    "register an input handler" in {
      val dt = new DataTimeout
      dt must notBeNull
      val mc = mock[MessageChannel]
      dt.setMessageChannel(mc)
      dt.configure(config)
      mc.consume(Matchers.eq("input"), any(classOf[InputHandler])) was called
    }
    
    "publish messages onward" in {
      val dt = new DataTimeout
      var h : InputHandler = null
      val mc = new MockMessageChannel()
      dt.setMessageChannel(mc)
      dt.configure(config)
      val m = mock[InputMessage]
      mc.inject("input", m)
      mc.outputs.size() must_== 1
      val out = mc.outputs.get(0)
      out.channel must_== "output"
      out.msg must_== m
    }
  }
}
