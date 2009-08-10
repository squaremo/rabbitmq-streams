package com.rabbitmq.streams.plugins.notification

import org.specs._
import org.specs.runner.{ConsoleRunner, JUnit4}
import net.sf.json.JSONObject
import com.rabbitmq.streams.harness.PluginBuildException
import com.rabbitmq.streams.harness.PluginException
import com.rabbitmq.streams.harness.MessageChannel
import com.rabbitmq.streams.harness.InputHandler
import com.rabbitmq.streams.harness.Message

import org.mockito.Mockito
import org.mockito.Matchers
import com.rabbitmq.streams.harness.AMQPConnectionFactory
import com.rabbitmq.client.Connection
import com.rabbitmq.streams.harness.testsupport.MockMessageChannel

class MySpecTest extends JUnit4(NotificationSpec)
//class MySpecSuite extends ScalaTestSuite(MySpec)
object MySpecRunner extends ConsoleRunner(NotificationSpec)

object NotificationSpec extends Specification {
  def config(host : String, vhost : String, port : Int, username : String, password : String) : JSONObject = {
    val c = new JSONObject
    c.put("host", host)
    c.put("virtual_host", vhost)
    c.put("port", port)
    c.put("username", username)
    c.put("password", password)
    c
  }

  "Notification server" should {
    "be able to be constructed" in {
      val plugin = new NotificationServer
      plugin must notBeNull
    }

  }

  "Notification server" should {
    "complain if it gets a duff AMQP connection config" in {
      val plugin = new NotificationServer
      try {
        plugin.configure(config("duffhost.doesntexist", "/", 5672, "", ""))
        fail("Didn't throw an exception when given bad configuration")
      }
      catch {
        case ex : PluginBuildException => ; // we want to see this
        case ex => fail("Some other exception thrown: " + ex.toString)
      }
    }

    "complain if it hasn't been configured" in {
      try {
        val plugin = new NotificationServer
        try {
          plugin.terminalStatusChange("string", new java.util.ArrayList(), true)
          fail("Didn't complain about not having a connection")
        }
        catch {
          case ex : PluginException =>; // what we're expecting
          case ex => fail("Expected PluginException, got " + ex.toString)
        }
      }
    }

    "register a command handler" in {
      val plugin = new NotificationServer
      val mc = new MockMessageChannel
      plugin.setMessageChannel(mc)
      val cf = Mockito.mock(classOf[AMQPConnectionFactory])
      val c = Mockito.mock(classOf[Connection])
      Mockito.when(cf.connectionFromConfig(Matchers.any(classOf[JSONObject]))).thenReturn(c)
      plugin.setConnectionFactory(cf)
      plugin.configure(new JSONObject())
      mc.handlers.get("command") must notBeNull
    }

    "complain if given an empty terminalConfig" in {
      val plugin = new NotificationServer
      val configs = new java.util.ArrayList[JSONObject]()
      configs.add(new JSONObject())
      try {
        plugin.terminalStatusChange("string", configs, true)
        fail("Didn't complain about empty terminal configuration")
      }
      catch {
        case ex => ex.isInstanceOf[PluginException] must beTrue
      }
    }
  }
}

