package com.rabbitmq.streams.harness;

import com.rabbitmq.client.QueueingConsumer;
import com.rabbitmq.client.impl.ChannelN;
import net.sf.json.JSONObject;
import net.sf.json.JSONArray;

import java.util.Map;

public abstract class InputReaderRunnable implements Runnable {

  public static final String PLUGIN_VALUES_HEADER = "x-streams-plugin-values";

  public InputReaderRunnable() {
  }

  public void configure(QueueingConsumer consumer, ChannelN channel, InputHandler handler, JSONObject config, Logger log) {
    this.consumer = consumer;
    this.channel = channel;
    this.handler = handler;
    this.config = config;
    this.log = log;
    makeStaticConfiguration();
  }

  private void makeStaticConfiguration() {
    JSONArray globalConfig = config.getJSONObject("plugin_type").getJSONArray("global_configuration_specification");
    staticConfiguration = new JSONObject();
    for (Object configItem : globalConfig) {
      JSONObject item = (JSONObject) configItem;
      staticConfiguration.put(item.getString("name"), JSONObject.fromObject(item.get("value")));
    }
    staticConfiguration.putAll(config.getJSONObject("configuration"));
  }

    /**
   * Set values in the header.
   */
  protected static void setValuesInHeader(Map<String, Object> headersToMutate, JSONObject vals) {
    headersToMutate.put(PLUGIN_VALUES_HEADER, vals);
  }

  protected static JSONObject getValuesFromHeader(Map<String, Object> headers) {
    return (headers.containsKey(PLUGIN_VALUES_HEADER)) ? JSONObject.fromObject(headers.get(PLUGIN_VALUES_HEADER)) : null;
  }

  static JSONObject mergeConfigWithHeaders(JSONObject original, Map<String, Object> headers) {
    JSONObject result = JSONObject.fromObject(original);
    if (headers != null) {
      JSONObject values = getValuesFromHeader(headers);
      if (values != null) {
        result = interpolateConfig(original, values);
      }
    }

    return result;
  }

  /**
   * Interpolate values given in the header into the dynamic configuration.
   * This is so that upstream components can pass on calculated values, to
   * be used for handling a particular message.
   */
  protected static JSONObject interpolateConfig(JSONObject uninterpolated, Map<String, Object> vals) {
    JSONObject result = JSONObject.fromObject(uninterpolated);
    for (Object k : uninterpolated.keySet()) {
      String key = k.toString();
      String uninterpolatedValue = uninterpolated.getString(key);
      // This can largely be done statically, but I'm waiting until the harness
      // is refactored.
      if (uninterpolatedValue.startsWith("$")) {
        String valKey = uninterpolatedValue.substring(1);
        result.put(key, vals.containsKey(valKey) ? vals.get(valKey) : "");
      }
    }
    return result;
  }

  protected QueueingConsumer consumer;
  protected ChannelN channel;
  protected InputHandler handler;
  protected JSONObject config, staticConfiguration;
  protected Logger log;
}

