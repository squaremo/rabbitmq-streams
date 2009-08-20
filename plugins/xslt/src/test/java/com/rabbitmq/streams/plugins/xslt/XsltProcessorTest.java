package com.rabbitmq.streams.plugins.xslt;

import com.rabbitmq.streams.harness.*;
import java.io.UnsupportedEncodingException;
import java.util.logging.Level;
import net.sf.json.JSONObject;
import static org.junit.Assert.fail;
import org.junit.Before;
import org.junit.Test;
import static org.mockito.Mockito.*;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.net.URI;

public class XsltProcessorTest {

  private XsltProcessor processor = new XsltProcessor();
  private JSONObject config = new JSONObject();
  private MessageChannel channel;
  private InputMessage message;
  private Logger log;

  @Before
  public void setup() throws IOException {
    channel = mock(MessageChannel.class);
    message = mock(InputMessage.class);
    log = mock(Logger.class);

    String identityTransform = "<?xml version=\"1.0\"?>\n" +
      "<xsl:stylesheet xmlns:xsl=\"http://www.w3.org/1999/XSL/Transform\">\n" +
      "<xsl:template match=\"@*|node()\">\n" +
      "   <xsl:copy>\n" +
      "      <xsl:apply-templates select=\"@*|node()\"/>\n" +
      "   </xsl:copy>\n" +
      "</xsl:template>" +
      "</xsl:stylesheet>";
    URI stylesheetUri = createTemporaryStyleSheet(identityTransform);
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
    try {
      when(message.body()).thenReturn("<things></things>".getBytes("utf-8"));
    } catch (UnsupportedEncodingException ex) {
      throw new RuntimeException("This shouldn't happen.");
    }
    when(message.withBody("<?xml version=\"1.0\" encoding=\"UTF-8\"?><things/>")).thenReturn(outputMessage);
    try {
      processor.input.handleMessage(message);
      verify(channel).publish("output", outputMessage);
    }
    catch (PluginException e) {
      e.printStackTrace();
    }
  }

  @Test
  public void testTransformErrors() throws PluginBuildException {
    processor.setMessageChannel(channel);
    processor.configure(config);
    processor.setLog(log);

    try {
      when(message.body()).thenReturn("<things></broken>".getBytes("utf-8"));
    } catch (UnsupportedEncodingException ex) {
      throw new RuntimeException("This shouldn't happen.");
    }
    try {
      processor.input.handleMessage(message);
      fail("An exception should be thrown");
    }
    catch (PluginException ignore) {
    }
    verify(log, atLeastOnce()).error(any(Exception.class));
  }

  private URI createTemporaryStyleSheet(String content) throws IOException {
    File temp = File.createTempFile("transform", ".xsl");
    temp.deleteOnExit();

    BufferedWriter out = new BufferedWriter(new FileWriter(temp));
    out.write(content);
    out.close();

    return temp.toURI();
  }

}

