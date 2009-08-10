package com.rabbitmq.streams.plugin.archive

import org.specs.runner.JUnit4
import org.specs.Specification
import org.specs.mock.Mockito
import org.mockito.Matchers._
import com.rabbitmq.streams.harness.Logger
import com.fourspaces.couchdb.Session
import scala.actors.SingleThreadedScheduler
import net.sf.json.JSONObject
import com.fourspaces.couchdb.Database
import com.fourspaces.couchdb.Document


class DispatcherTest extends JUnit4(DispatcherSpec)
object DispatcherSpec extends Specification with Mockito {
  val log = mock[Logger]
  var session: Session = null
  var database: Database = null
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

    doBefore {
      session = mock[Session]
      database = mock[Database]
      acknowledged = false
      dispatcher = new TestDispatcher(log, session)
      dispatcher.start
    }

    doAfter  {
      dispatcher = null
    }

    val viewName = "_design/by_date"
    val databaseName = "archive_testtest"

    def createDatabase {
      session.createDatabase(databaseName) returns database
      session.getDatabase(databaseName) returns database

      val config = JSONObject.fromObject("""{"destination":{"name": "test"}}""")

      dispatcher ! DestinationStatusChange("test", List(config), true)

      session.createDatabase("archive_testtest") was called
    }

    "create a couch database for new active entry " in  {
      createDatabase
    }

    "create a bydate view in the couch database if none exists" in  {
      database.getDocument(viewName) returns null
      createDatabase
      database.getDocument(viewName) was called
      database.saveDocument(any(classOf[Document]), matches(viewName)) was called
    }

    "not create a bydate view in the couch database if one already exists" in  {
      database.getDocument(viewName) returns new Document()
      createDatabase
      database.getDocument(viewName) was called
      database.saveDocument(any(classOf[Document]), any(classOf[String])) was notCalled
    }

    "send to a new destination " in  {
      createDatabase
      dispatcher ! Entry(content, "test", ack)
      acknowledged must_== true
    }
  }
}
