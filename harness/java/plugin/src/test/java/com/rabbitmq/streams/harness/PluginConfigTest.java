/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.rabbitmq.streams.harness;

import com.rabbitmq.client.AMQP.BasicProperties;
import java.util.HashMap;
import java.util.Map;
import net.sf.json.JSONArray;
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
   * Test that interpolateConfig works in shallow cases
   */
  @Test
  public void testSimpleInterpolateConfig() {
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

  @Test
  public void testNestedMapInterpolateConfig() {
    Map<String, Object> vals = new HashMap();
    vals.put("foo", "bar");
    JSONObject conf = new JSONObject();
    JSONObject subconf = new JSONObject();
    subconf.put("here", "$foo");
    conf.put("underhere", subconf);
    JSONObject result = AMQPInputConsumer.interpolateConfig(conf, vals);
    assertNotNull(result);
    Object subresult = result.get("underhere");
    assertTrue(subresult instanceof JSONObject);
    JSONObject subresultjson = (JSONObject)subresult;
    assertEquals("bar", subresultjson.get("here"));
  }

//  @Test
  public void testArrayInterpolateConfig() {
    Map<String, Object> vals = new HashMap();
    vals.put("foo", "bar");
    JSONObject conf = new JSONObject();
    JSONArray subconf = new JSONArray();
    subconf.add("notthis");
    subconf.add("$foo");
    conf.put("underhere", subconf);
    JSONObject result = AMQPInputConsumer.interpolateConfig(conf, vals);
    assertNotNull(result);
    Object subresult = result.get("underhere");
    assertTrue(subresult instanceof JSONArray);
    JSONArray subresultjson = (JSONArray)subresult;
    assertEquals("notthis", subresultjson.get(0));
    assertEquals("bar", subresultjson.get(1));
  }

}
