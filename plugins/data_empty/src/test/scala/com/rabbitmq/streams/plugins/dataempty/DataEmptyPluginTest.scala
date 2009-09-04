package com.rabbitmq.streams.plugins.dataempty

import harness.testsupport.MockMessageChannel
import harness.{NotificationType, Notifier, InputMessage}
import net.sf.json.JSONObject
import org.specs.mock.Mockito
import org.specs.runner.JUnit4
import org.specs.Specification

class DataEmptyPluginTest extends JUnit4(DataEmptyPluginSpec)
object DataEmptyPluginSpec extends Specification with Mockito {
  val config = new JSONObject
  val bang = "BANG"
  config.put("message", bang)

  "notifies of bad data" in {
    val messageChannel = new MockMessageChannel
    val message = mock[InputMessage]
    val notifier = mock[Notifier]
    val plugin = new DataEmptyPlugin

    plugin.setNotifier(notifier)
    plugin.setMessageChannel(messageChannel)
    plugin.configure(config)

    messageChannel.inject("input", message)

    notifier.notify(NotificationType.BadData, bang) was called
  }
}