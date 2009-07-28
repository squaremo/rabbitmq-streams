package com.rabbitmq.streams.xmpppubsub

import net.sf.json.JSONObject
import org.specs._
import org.specs.runner.JUnit
import org.mockito.Mockito.{mock => mockOf}

import com.rabbitmq.streams.harness.MessageChannel

class XmppPubSubTest extends Specification with JUnit {
  "xmpppubsub" should {
    "throw an exception if its configuration isn't present" in {
      {
      val server = new XmppPubSubServer()
      val mc = mockOf(classOf[MessageChannel])
      server.setMessageChannel(mc)
      server.configure(hostAndPort)
      ()
      } must throwAn[net.sf.json.JSONException]
    }
    "read proxy configuration from json" in {
      // TODO!
      "x".size must_== 1
    }
    "produce an error if neither simple or proxy configuration available" in {
      // TODO!
      {throw new Error("BANG"); ()} must throwAn[Error]
    }
  }

  def hostAndPort: JSONObject = JSONObject.fromObject(
    """
{
  "plugin_type": {
    "global_configuration_specification": [ {"name": "test"} ]
   },
   configuration: {},
   messageserver: {
    "virtual_host": "vhost",
    "username": "test",
    "password": "password",
    "host": "localhost",
    "port": "9812"

   }
}
    """
    )
}