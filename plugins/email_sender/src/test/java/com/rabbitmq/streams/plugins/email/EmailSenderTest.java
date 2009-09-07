package com.rabbitmq.streams.plugins.email;

import junit.framework.TestCase;
import com.rabbitmq.streams.harness.testsupport.MockMessageChannel;
import com.rabbitmq.streams.harness.*;
import static org.mockito.Mockito.*;
import net.sf.json.JSONObject;

import java.util.List;
import java.util.ArrayList;
import java.io.IOException;

public class EmailSenderTest extends TestCase {
  private JSONObject config;

  public void setUp() {
    config = new JSONObject();
    config.put("host", "0.0.0.0");
    config.put("username", "username");
    config.put("password", "password");
    config.put("transportProtocol", "smtp");

  }


  public void testNotification() throws IOException {
    InputMessage message = mock(InputMessage.class);
    when(message.routingKey()).thenReturn("bang");
    when(message.body()).thenReturn("bang".getBytes());

    MockMessageChannel messageChannel = new MockMessageChannel();

    EmailSender emailSender = new EmailSender();
    emailSender.setMessageChannel(messageChannel);
    Notifier notifier = mock(Notifier.class);
    emailSender.setNotifier(notifier);
    Logger log = mock(Logger.class);
    emailSender.setLog(log);

    DatabaseResource database = mock(DatabaseResource.class);
    when(database.getDocument(anyString())).thenReturn(new JSONObject());
    emailSender.setTerminalsDatabase(database);

    String destination = "{\"destination\":{\"to\":[\"qwe@asd.zxc\"],\"subject\":\"simple test\"}}";
    JSONObject terminal = JSONObject.fromObject(destination);
    List<JSONObject> terminalConfigs = new ArrayList<JSONObject>();
    terminalConfigs.add(terminal);

    try {
      emailSender.configure(config);
      emailSender.terminalStatusChange("bang", terminalConfigs, true);

    }
    catch (PluginBuildException e) {
      e.printStackTrace();
    }

    try {
      messageChannel.inject("input", message);
      fail();
    }
    catch (PluginException ignore) {
    }

    verify(notifier).notify(NotificationType.Unavailable, "Could not deliver email 501 5.1.7 Bad sender address syntax");
  }
}
