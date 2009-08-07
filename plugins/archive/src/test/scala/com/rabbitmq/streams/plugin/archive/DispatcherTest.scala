package com.rabbitmq.streams.plugin.archive

import org.specs.runner.JUnit4
import org.specs.Specification
import org.specs.mock.Mockito
import com.rabbitmq.streams.harness.Logger
import com.fourspaces.couchdb.Session
import scala.actors.SingleThreadedScheduler
import net.sf.json.JSONObject


class DispatcherTest extends JUnit4(DispatcherSpec)
object DispatcherSpec extends Specification with Mockito {
  val log = mock[Logger]
  val session = mock[Session]
  val string = "12345"
  val content:Array[Byte] = string.getBytes
 
  class TestDispatcher(log:Logger, session:Session) extends Dispatcher(log, session)  {
    override val scheduler = new SingleThreadedScheduler
  }


  var dispatcher:Dispatcher = null

  "The dispatcher" should  {
    var acknowledged = false
    def ack():Unit =  {
      acknowledged = true
    }

    doFirst {
      acknowledged = false
      dispatcher = new TestDispatcher(log, session)
      dispatcher.start
    }

    doLast  {
      dispatcher = null
    }

    "create a couch database for new active entry " in  {
      val config = JSONObject.fromObject("""{"destination":{"name": "test"}}""")
      dispatcher ! DestinationStatusChange("dest1", List(config), true)
      session.createDatabase("archive_dest1test") was called
    }

    "send to a new destination " in  {
      val config = JSONObject.fromObject("""{"destination":{"name": "test"}}""")
      dispatcher ! DestinationStatusChange("dest1", List(config), true)
      session.createDatabase("archive_dest1test") was called

      dispatcher ! Entry(content, "dest1", ack)
      acknowledged must_== true
    }
  }

  override def main(args: Array[String])  {

  }
}
