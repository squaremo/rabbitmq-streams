package com.rabbitmq.streams.plugins.javascript

import org.specs._
import org.specs.runner.{ConsoleRunner, JUnit4}

import net.sf.json.JSONObject

class PluginSpecTest extends JUnit4(PluginSpec)
//class MySpecSuite extends ScalaTestSuite(MySpec)
object PluginSpecRunner extends ConsoleRunner(PluginSpec)

object PluginSpec extends Specification {
  "JavaScript plugin" should {
    "compile the function it's given" in {
      val plugin = new JavaScriptPlugin()
      plugin must notBeNull
    }
  }
}
