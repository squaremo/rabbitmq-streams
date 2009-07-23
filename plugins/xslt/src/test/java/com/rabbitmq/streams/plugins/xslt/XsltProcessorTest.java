package com.rabbitmq.streams.plugins.xslt;

import org.junit.Test;
import org.junit.Before;
import static org.mockito.Mockito.*;
import static org.junit.Assert.*;
import net.sf.json.JSONObject;
import com.rabbitmq.streams.harness.*;

import java.io.IOException;
import java.io.FileWriter;
import java.io.BufferedWriter;
import java.io.File;
import java.net.URI;
import java.util.Map;

public class XsltProcessorTest {

  private XsltProcessor processor = new XsltProcessor();
  private JSONObject config = new JSONObject();
  private MessageChannel channel;
  private InputMessage message;
  private URI stylesheetUri;

  @Before
  public void setup() throws IOException {
    channel = mock(MessageChannel.class);
    message = mock(InputMessage.class);

    stylesheetUri = createTemporaryStyleSheet();
    config.put("stylesheet_url", stylesheetUri.toASCIIString());
  }

  @Test
  public void testNullConfig() {
    try {
      processor.configure(null);
      fail("Shouldn't build with null configuration");
    }
    catch (PluginBuildException ignore) {
    }
  }

  @Test
  public void testTransform() throws PluginBuildException {
    processor.setMessageChannel(channel);
    processor.configure(config);

    InputMessage outputMessage = mock(InputMessage.class);
    when(message.body()).thenReturn("<things></things>".getBytes());
    when(message.withBody("<?xml version=\"1.0\" encoding=\"UTF-8\"?><things/>")).thenReturn(outputMessage);
    try {
      processor.input.handleMessage(message, null);
      verify(channel).publish("output", outputMessage);
    }
    catch (PluginException e) {
      e.printStackTrace();
    }
  }

  private URI createTemporaryStyleSheet() throws IOException {
    File temp = File.createTempFile("transform", ".xsl");
    temp.deleteOnExit();

    BufferedWriter out = new BufferedWriter(new FileWriter(temp));

    String identityTransform = "<?xml version=\"1.0\"?>\n" +
      "<xsl:stylesheet xmlns:xsl=\"http://www.w3.org/1999/XSL/Transform\">\n" +
      "<xsl:template match=\"@*|node()\">\n" +
      "   <xsl:copy>\n" +
      "      <xsl:apply-templates select=\"@*|node()\"/>\n" +
      "   </xsl:copy>\n" +
      "</xsl:template>" +
      "</xsl:stylesheet>";

    out.write(identityTransform);
    out.close();

    return temp.toURI();
  }

  private class TestMessage extends InputMessage {

    public InputMessage withHeader(String s, Object o) {
      return null;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public InputMessage withBody(byte[] bytes) {
      return null;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public InputMessage withHeaders(Map<String, Object> stringObjectMap) {
      return null;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public void ack() throws MessagingException {
      //To change body of implemented methods use File | Settings | File Templates.
    }

    public Map<String, Object> headers() {
      return null;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public byte[] body() {
      return new byte[0];  //To change body of implemented methods use File | Settings | File Templates.
    }

    public String routingKey() {
      return null;  //To change body of implemented methods use File | Settings | File Templates.
    }
  }

}

