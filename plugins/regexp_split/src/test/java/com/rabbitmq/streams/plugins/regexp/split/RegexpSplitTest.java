package com.rabbitmq.streams.plugins.regexp.split;

import org.junit.Test;
import org.junit.Before;
import static org.mockito.Mockito.*;
import static org.junit.Assert.*;
import com.rabbitmq.streams.harness.PluginBuildException;
import com.rabbitmq.streams.harness.MessageChannel;
import com.rabbitmq.streams.harness.InputMessage;
import com.rabbitmq.streams.harness.PluginException;
import net.sf.json.JSONObject;

public class RegexpSplitTest {
  private RegexpSplit splitter;
  private JSONObject config = new JSONObject();
  private MessageChannel channel;
  private InputMessage message;

  private String testString = "sausages";

  @Before
  public void setup() {
    config.put("regexp", testString);
    config.put("multiline", true);
    config.put("caseinsensitive", true);
    config.put("dotall", true);
    splitter = new RegexpSplit();

    channel = mock(MessageChannel.class);
    message = mock(InputMessage.class);
  }

  @Test
  public void testConfigureWithNullConfig() {
    try {
      splitter.configure(null);
      fail("Cannot configure a plugin with a null configuration");
    }
    catch (PluginBuildException ignore) {
    }
  }

  @Test
  public void testConfigureWithNoRegexp() {
    // TODO
  }

  @Test
  public void testConfigure() throws PluginBuildException {
    splitter.setMessageChannel(channel);
    splitter.configure(config);
  }

  @Test
  public void testPositiveMessage() throws PluginBuildException {
    splitter.setMessageChannel(channel);
    splitter.configure(config);

    when(message.body()).thenReturn(testString.getBytes());
    try {
      splitter.input.handleMessage(message);
      verify(channel).publish(RegexpSplit.POSITIVE, message);
    }
    catch (PluginException e) {
      e.printStackTrace();
    }
  }

  @Test
  public void testNegativeMessage() throws PluginBuildException {
    splitter.setMessageChannel(channel);
    splitter.configure(config);

    when(message.body()).thenReturn("bangers".getBytes());
    try {
      splitter.input.handleMessage(message);
      verify(channel).publish(RegexpSplit.NEGATIVE, message);
    }
    catch (PluginException e) {
      e.printStackTrace();
    }
  }
}
