package com.rabbitmq.streams.plugin.archive

import harness.{PluginBuildException, MessageChannel}
import org.specs.Specification
import org.specs.mock.Mockito
import org.specs.runner.JUnit4
import net.sf.json.JSONObject

class ArchiveTest extends JUnit4(ArchiveSpec)
object ArchiveSpec extends Specification with Mockito {
  val server = new ArchiveServer()
  val channel = mock[MessageChannel]

  server.setMessageChannel(channel)

  "This test" should {
    "use mocks" in {
      val m = mock[java.util.List[String]]
      m.get(0) returns "one"

      m.get(0)

      m.get(0) was called
      m.get(1) was notCalled
    }
  }

  "The archive server" should {
    "not configure itself from a null configuration" in  {
      server must_!= null
      server.configure(null) must throwA[PluginBuildException]
    }
    "use defaults if no configuration values found " in {
      server must_!= null
      server.configure(new JSONObject)
    }
  }
}