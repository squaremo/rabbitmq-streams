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
   * Test of setValuesInHeader method, of class Plugin.
   */
  @Test
  public void testSetValuesInHeader() {
    Map<String, Object> header = new HashMap();
    JSONObject vals = new JSONObject();
    vals.put("foo", "bar");
    InputConsumer.setValuesInHeader(header, vals);
    assertEquals(InputConsumer.getValuesFromHeader(header), vals);
    assertEquals(InputConsumer.getValuesFromHeader(header).getString("foo"), "bar");
  }

  public class PluginImpl extends Plugin {

    InputConsumer handlerRunnable(String name) {
      return null;
    }
  }

}
