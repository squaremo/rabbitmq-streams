/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.rabbitmq.streams.harness;

import com.rabbitmq.client.AMQP.BasicProperties;
import java.util.HashMap;
import java.util.Map;
import org.junit.Test;
import static org.junit.Assert.*;
import net.sf.json.JSONObject;

/**
 *
 * @author mikeb
 */
public class PluginConfigTest {

  /**
   * Test of propertiesWithHeaders method, of class Plugin.
   */
  @Test
  public void testPropertiesWithHeaders() {
    Map<String, Object> hdrs = new HashMap();
    hdrs.put("foo", "bar");
    BasicProperties props = AMQPPublisher.propertiesWithHeaders(hdrs);
    assertNotNull(props);
    assertNotNull(props.headers);
    assertEquals(props.deliveryMode, new Integer(2));
    assertEquals(props.headers.get("foo"), "bar");
  }

  @Test
  public void testSetValues() {
    Map<String, Object> headers = new HashMap();
    JSONObject vals = new JSONObject();
    AMQPInputConsumer.setValuesInHeader(headers, vals);
    assertEquals(vals, AMQPInputConsumer.getValuesFromHeader(headers));
  }

  /**
   * Test that interpolateConfig
   */
  @Test
  public void testInterpolateConfig() {
    Map<String, Object> vals = new HashMap();
    vals.put("foo", "bar");
    JSONObject conf = new JSONObject();
    conf.put("interpolated", "$foo");
    conf.put("notinterpolated", "foo");
    JSONObject interpolateConfig = AMQPInputConsumer.interpolateConfig(conf, vals);
    assertNotNull(interpolateConfig);
    assertEquals(interpolateConfig.size(), 2);
    assertEquals("bar", interpolateConfig.get("interpolated"));
    assertEquals("foo", interpolateConfig.get("notinterpolated"));
  }

}
