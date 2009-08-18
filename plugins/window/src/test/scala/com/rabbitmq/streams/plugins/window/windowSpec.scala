

package com.rabbitmq.streams.plugins.window

import net.sf.json._
import com.rabbitmq.streams.harness.{MessageChannel, InputHandler}

import org.specs._
import org.specs.mock.Mockito
import org.mockito.Matchers._
import org.mockito.Matchers
import org.specs.runner.{ConsoleRunner, JUnit4}
import com.rabbitmq.streams.harness.testsupport.MockMessageChannel
import com.rabbitmq.streams.harness.InputMessage
import com.rabbitmq.streams.harness.StateResource
import com.rabbitmq.streams.harness.PluginBuildException
import com.rabbitmq.streams.harness.PipelineComponent
import com.rabbitmq.streams.harness.Logger
import com.rabbitmq.streams.harness.{Notifier, NotificationType}

class WindowTest extends JUnit4(WindowSpec)
//class MySpecSuite extends ScalaTestSuite(WindowSpec)
object MySpecRunner extends ConsoleRunner(WindowSpec)


object WindowSpec extends Specification with Mockito {

  def fullyMock(plugin : PipelineComponent) {
    plugin.setLog(mock[Logger])
    plugin.setMessageChannel(mock[MessageChannel])
    plugin.setNotifier(mock[Notifier])
    plugin.setStateResource(mock[StateResource])
  }

  "Window" should {

    val win = new WindowPlugin
    fullyMock(win)
    val config = JSONObject.fromObject(
      """{"timeout": null, "count":7,
          "overlap":0, "unit":"bytes", "encoding":"utf-8-sep:."}""")

    "be nullary-constructable" in {
      win must notBeNull
    }

    "not complain given OK values" in {
      win.configure(config)
    }

    //XXX Actually I don't care about testing this here, that should be handled
    //in the plugin.js veryfier
//    "complain about a nonsense timeout value" in {
//      val c = JSONObject.fromObject(config)
//      c.put("timeout", "zappa")
//      win.configure(c) must throwA[PluginBuildException]
//    }

    "register an input handler" in {
      win must notBeNull
      val mc = mock[MessageChannel]
      win.setMessageChannel(mc)
      win.configure(config)
      mc.consume(Matchers.eq("input"), any(classOf[InputHandler])) was called
    }

    //FIXME(alexander): figure out why the test below fails

    // "publish full message onward" in {
    //   val mc = new MockMessageChannel()
    //   win.setMessageChannel(mc)
    //   win.configure(config)
    //   val m = mock[InputMessage]
    //   m.body returns "1234567".getBytes("utf-8")
    //   m.bodyAsString returns "1234567"
    //   System.err.println("###DEBUG" + m.bodyAsString)
    //   mc.inject("input", m)
    //   mc.outputs.size() must_== 1
    //   val out = mc.outputs.get(0)
    //   System.err.println("###DEBUG"+out.msg)
    //   out.channel must_== "output"
    //   out.msg must_== m
    // }

    "" in {

    }

    }
}
