package com.rabbitmq.streams.harness;

import com.rabbitmq.client.QueueingConsumer;
import com.rabbitmq.client.impl.ChannelN;
import net.sf.json.JSONObject;
import net.sf.json.JSONArray;

import java.util.Map;

public abstract class InputReaderRunnable implements Runnable {

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

  protected JSONObject mergeConfigWithHeaders(JSONObject original, Map<String, Object> headers) {
    JSONObject result = JSONObject.fromObject(original);
    if (headers != null) {
      JSONObject values = headerValues(headers);
      if (values != null) {
        for (Object k : values.keySet()) {
          String key = (String) k;
          String originalValue = original.getString(key);
          if (originalValue.startsWith("$")) {
            String variableName = originalValue.substring(1);
            if (values.containsKey(variableName)) {
              result.put(key, values.get(variableName));
            }
            else {
              result.put(key, ""); // No variable defined in headers so blank field.
            }
          }
        }
      }
    }

    return result;
  }

  private JSONObject headerValues(Map<String, Object> headers) {
    if (headers.containsKey(X_STREAMS_PLUGIN_VALUES)) {
      return JSONObject.fromObject(headers.get(X_STREAMS_PLUGIN_VALUES));
    }
    return null;
  }

  private static final String X_STREAMS_PLUGIN_VALUES = "x-streams-plugin-values";
  protected QueueingConsumer consumer;
  protected ChannelN channel;
  protected InputHandler handler;
  protected JSONObject config, staticConfiguration;
  protected Logger log;
}

