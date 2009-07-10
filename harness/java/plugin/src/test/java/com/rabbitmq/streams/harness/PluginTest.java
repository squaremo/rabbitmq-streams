/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.rabbitmq.streams.harness;

import com.rabbitmq.client.QueueingConsumer.Delivery;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import net.sf.json.JSONObject;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.*;

import static org.mockito.Mockito.*;

/**
 *
 * @author mikeb
 */
public class PluginTest extends PluginTestingTools {

    public PluginTest() {
    }

  @BeforeClass
  public static void setUpClass() throws Exception {
  }

  @AfterClass
  public static void tearDownClass() throws Exception {
  }

    @Before
    public void setUp() {
    }

    @After
    public void tearDown() {
    }

  /**
   * Test of configuredCorrectly method, of class Plugin.
   */
    //@Test
  public void testConfiguredCorrectly() {
    // TODO test base cases of incorrect config
  }

  /**
   * Test of addOutput method, of class Plugin.
   */
  @Test
  public void testAddOutput() throws IOException {
    Publisher pub = mock(Publisher.class);
    Plugin plugin = new NoopPlugin();
    plugin.addOutput("foo", pub);
    plugin.getPublisher("foo").publish("foobar".getBytes());
    verify(pub).publish("foobar".getBytes());
  }

  /**
   * Test that if we register a handler, then send something to the plugin's idea of the handler, it goes
   * to the same place.
   */
  @Test
  public void testRegisterHandler() throws Exception {
    InputReader handler = mock(InputReader.class);
    Plugin plugin = new NoopPlugin();
    plugin.registerHandler("foo", handler);
    Delivery parcel = new Delivery(null, null, "bar".getBytes());
    plugin.handler("foo").handleDelivery(parcel, null);
    verify(handler).handleDelivery(parcel, null);
  }

  /**
   * Test of setValuesInHeader method, of class Plugin.
   */
  @Test
  public void testSetValuesInHeader() {
    Map<String, Object> header = new HashMap();
    JSONObject vals = new JSONObject();
    vals.put("foo", "bar");
    InputReaderRunnable.setValuesInHeader(header, vals);
    assertEquals(InputReaderRunnable.getValuesFromHeader(header), vals);
    assertEquals(InputReaderRunnable.getValuesFromHeader(header).getString("foo"), "bar");
  }

  public class PluginImpl extends Plugin {

    public PluginImpl() throws Exception {
      super(null);
    }

    public InputReaderRunnable handlerRunnable(String name) {
      return null;
    }
  }

}
