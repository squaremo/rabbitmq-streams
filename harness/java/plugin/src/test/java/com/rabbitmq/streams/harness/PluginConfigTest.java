package com.rabbitmq.streams.harness;

import com.rabbitmq.client.AMQP.BasicProperties;
import com.rabbitmq.client.QueueingConsumer;
import java.util.HashMap;
import java.util.Map;
import net.sf.json.JSONArray;
import net.sf.json.JSONNull;
import org.junit.Test;
import static org.junit.Assert.*;
import net.sf.json.JSONObject;

import org.junit.Before;
import static org.mockito.Mockito.*;

/**
 *
 * @author mikeb
 */
public class PluginConfigTest {

  JSONObject values;
  JSONObject conf;
  QueueingConsumer queue;
  Logger log;
  InputHandler handler;
  Map<String, Object> headers;

  @Before
  public void setUp() {
    values = new JSONObject();
    conf = new JSONObject();
    queue = mock(QueueingConsumer.class);
    log = mock(Logger.class);
    handler = mock(InputHandler.class);
    headers = new HashMap();
  }

  /**
   * Test of propertiesWithHeaders method, of class Plugin.
   */
  @Test
  public void testPropertiesWithHeaders() {
    headers.put("foo", "bar");
    BasicProperties props = AMQPPublisher.propertiesWithHeaders(headers);
    assertNotNull(props);
    assertNotNull(props.headers);
    assertEquals(props.deliveryMode, new Integer(2));
    assertEquals(props.headers.get("foo"), "bar");
  }

  @Test
  public void testSetValues() {
    AMQPInputConsumer.setValuesInHeader(headers, values);
    assertEquals(values, AMQPInputConsumer.getValuesFromHeader(headers));
  }

  /**
   * Test that interpolateConfig works in shallow cases
   */
  @Test
  public void testSimpleInterpolateConfig() {
    values.put("foo", "bar");
    conf.put("interpolated", "$foo");
    conf.put("notinterpolated", "foo");
    JSONObject interpolateConfig = AMQPInputConsumer.interpolateConfig(conf, values);
    assertNotNull(interpolateConfig);
    assertEquals(interpolateConfig.size(), 2);
    assertEquals("bar", interpolateConfig.get("interpolated"));
    assertEquals("foo", interpolateConfig.get("notinterpolated"));
  }

  @Test
  public void testNestedMapInterpolateConfig() {
    values.put("foo", "bar");
    JSONObject subconf = new JSONObject();
    subconf.put("here", "$foo");
    conf.put("underhere", subconf);
    JSONObject result = AMQPInputConsumer.interpolateConfig(conf, values);
    assertNotNull(result);
    Object subresult = result.get("underhere");
    assertTrue(subresult instanceof JSONObject);
    JSONObject subresultjson = (JSONObject)subresult;
    assertEquals("bar", subresultjson.get("here"));
  }

  @Test
  public void testArrayInterpolateConfig() {
    values.put("foo", "bar");
    JSONArray subconf = new JSONArray();
    subconf.add("notthis");
    subconf.add("$foo");
    conf.put("underhere", subconf);
    JSONObject result = AMQPInputConsumer.interpolateConfig(conf, values);
    assertNotNull(result);
    Object subresult = result.get("underhere");
    assertTrue(subresult instanceof JSONArray);
    JSONArray subresultjson = (JSONArray)subresult;
    assertEquals("notthis", subresultjson.get(0));
    assertEquals("bar", subresultjson.get(1));
  }

  @Test
  public void testNoopMerge() {
    AMQPInputConsumer ic = new AMQPInputConsumer(queue, handler, conf, log) {

      public void run() {
        throw new UnsupportedOperationException("Not supported yet.");
      }

    };
    JSONObject merged = ic.mergeConfigWithHeaders(headers);
    assertNotNull(merged);
    assertTrue( ! JSONNull.getInstance().equals(merged));
  }

  @Test
  public void testTrivialMerge() {
    conf.put("foo", "bar");
    AMQPInputConsumer ic = new AMQPInputConsumer(queue, handler, conf, log) {
      public void run() {
        throw new UnsupportedOperationException("Not supported yet.");
      }
    };
    JSONObject merged = ic.mergeConfigWithHeaders(headers);
    assertNotNull(merged);
    assertTrue( ! JSONNull.getInstance().equals(merged));
    assertEquals(conf, merged);
  }

  @Test
  public void testMergeWithMissingValue() {
    conf.put("foo", "$bar");
    AMQPInputConsumer ic = new AMQPInputConsumer(queue, handler, conf, log) {
      public void run() {
        throw new UnsupportedOperationException("Not supported yet.");
      }
    };
    JSONObject merged = ic.mergeConfigWithHeaders(headers);
    assertNotNull(merged);
    assertTrue( ! JSONNull.getInstance().equals(merged));
    assertEquals("", merged.get("foo"));
  }

  @Test
  public void testMergeWithValue() {
    conf.put("foo", "$bar");
    values.put("bar", "BAZ");
    AMQPInputConsumer.setValuesInHeader(headers, values);
    AMQPInputConsumer ic = new AMQPInputConsumer(queue, handler, conf, log) {
      public void run() {
        throw new UnsupportedOperationException("Not supported yet.");
      }
    };
    JSONObject merged = ic.mergeConfigWithHeaders(headers);
    assertNotNull(merged);
    assertTrue( ! JSONNull.getInstance().equals(merged));
    assertEquals("BAZ", merged.get("foo"));
  }

}
