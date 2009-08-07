package com.rabbitmq.streams.plugin.archive

import org.specs.runner.JUnit4
import org.specs.Specification
import org.specs.mock.Mockito
import org.mockito.Matchers._
import scala.actors.SingleThreadedScheduler
import com.rabbitmq.streams.harness.Logger
import com.fourspaces.couchdb.Database
import com.fourspaces.couchdb.Document

class DestinationTest extends JUnit4(DestinationSpec)
object DestinationSpec extends Specification with Mockito {
  val log = mock[Logger]
  val database = mock[Database]
  val string = "12345"
  val content:Array[Byte] = string.getBytes

  val destination = new Destination(log, database) {
    override val scheduler = new SingleThreadedScheduler 
  }
  destination.start

  "The destination" should {
    "store the message in the database" in {
      destination ! NewEntry(content)
      database.saveDocument(any(classOf[Document])) was called
    }
  }
}
