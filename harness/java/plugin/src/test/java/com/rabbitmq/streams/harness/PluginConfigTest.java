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
    BasicProperties props = Publisher.propertiesWithHeaders(hdrs);
    assertNotNull(props);
    assertNotNull(props.headers);
    assertEquals(props.deliveryMode, new Integer(2));
    assertEquals(props.headers.get("foo"), "bar");
  }

  @Test
  public void testSetValues() {
    Map<String, Object> headers = new HashMap();
    JSONObject vals = new JSONObject();
    Plugin.setValuesInHeader(headers, vals);
    assertEquals(vals, Plugin.getValuesFromHeader(headers));
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
    JSONObject interpolateConfig = Plugin.interpolateConfig(conf, vals);
    assertNotNull(interpolateConfig);
    assertEquals(interpolateConfig.size(), 2);
    assertEquals("bar", interpolateConfig.get("interpolated"));
    assertEquals("foo", interpolateConfig.get("notinterpolated"));
  }

  /**
   * Test of configForHeaders method, of class Plugin.
   */
  @Test
  public void testConfigForHeaders() throws Exception {
    Map<String, Object> headers = new HashMap();
    Plugin.setValuesInHeader(headers, JSONObject.fromObject("{\"foo\": \"baz\"}"));
    JSONObject config = minimalConfig();
    JSONObject pluginconfig = new JSONObject();
    pluginconfig.put("foo", "$foo");
    pluginconfig.put("bar", "bar");
    config.put("configuration", pluginconfig);
    Plugin plugin = new Plugin(config) {
      public InputReaderRunnable handlerRunnable(String channel) {
        return null;
      }
    };
    plugin.setLog(new Logger(null, "") {
      @Override public void debug(String msg) {}
    });
    assertNotNull(plugin);
    JSONObject configForHeaders = plugin.configForHeaders(headers);
    assertNotNull(configForHeaders);
    assertEquals("baz", configForHeaders.get("foo"));
  }

  public JSONObject minimalConfig() {
    JSONObject res = new JSONObject();
    JSONObject ptype = new JSONObject();
    ptype.put("global_configuration_specification", new JSONArray());
    res.put("plugin_type", ptype);
    res.put("configuration", new JSONObject());
    return res;
  }

}
