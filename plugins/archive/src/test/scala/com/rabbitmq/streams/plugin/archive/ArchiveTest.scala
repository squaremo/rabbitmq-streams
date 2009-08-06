package com.rabbitmq.streams.plugin.archive

import org.specs.Specification
import org.specs.mock.Mockito
import org.specs.runner.JUnit4

class ArchiveTest extends JUnit4(ArchiveSpec)
object ArchiveSpec extends Specification with Mockito {
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
    "configure itself correctly" in  {
      "test".size must_==4
    }
  }
}