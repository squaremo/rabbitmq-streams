package com.rabbitmq.streams.plugins.regexp.replace;

import java.nio.charset.CharacterCodingException;

import org.junit.Test;
import org.junit.Before;
import static org.mockito.Mockito.*;
import static org.junit.Assert.*;
import com.rabbitmq.streams.harness.PluginBuildException;
import com.rabbitmq.streams.harness.MessageChannel;
import com.rabbitmq.streams.harness.InputMessage;
import com.rabbitmq.streams.harness.PluginException;
import net.sf.json.JSONObject;

public class RegexpReplaceTest {
  private RegexpReplace replacer;
  private JSONObject config = new JSONObject();
  private MessageChannel channel;
  private InputMessage message;

  private String testString = "foof";

  @Before
  public void setup() {
    replacer = new RegexpReplace();
    config = JSONObject.fromObject("{\"expressions\": [{\"regexp\":\"foo\",\"replacement\":\"bar\"},"+
                                       "{\"regexp\":\"barf\",\"replacement\":\"burp\"}]}");
    channel = mock(MessageChannel.class);
    message = mock(InputMessage.class);
  }

  @Test
  public void testConfigureWithNullConfig() {
    try {
      replacer.configure(null);
      fail("Cannot configure a plugin with a null configuration");
    }
    catch (PluginBuildException ignore) {
    }
  }


  @Test
  public void testConfigure() throws PluginBuildException {
    replacer.setMessageChannel(channel);
    replacer.configure(config);
  }


  @Test
  public void testNoReplacement() throws Exception {
    InputMessage output = mock(InputMessage.class);
    replacer.setMessageChannel(channel);
    replacer.configure(config);
    when(message.bodyAsString()).thenReturn("nothing to replace");
    when(message.withBody("nothing to replace")).thenReturn(output);
    replacer.input.handleMessage(message);
    verify(channel).publish("output", output);
  }

  @Test
  public void testReplacement() throws Exception {
    InputMessage output = mock(InputMessage.class);
    replacer.setMessageChannel(channel);
    replacer.configure(config);
    when(message.bodyAsString()).thenReturn("foof");
    when(message.withBody("burp")).thenReturn(output);
    replacer.input.handleMessage(message);
    verify(channel).publish("output",  output);
  }
}
