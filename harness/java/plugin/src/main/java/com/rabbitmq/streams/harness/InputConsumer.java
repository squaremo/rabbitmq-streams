package com.rabbitmq.streams.harness;

import com.rabbitmq.client.Channel;
import com.rabbitmq.client.QueueingConsumer;
import com.rabbitmq.client.QueueingConsumer.Delivery;
import java.io.IOException;
import net.sf.json.JSONObject;
import net.sf.json.JSONArray;

import java.util.Map;

abstract class InputConsumer implements Runnable {

  public static final String PLUGIN_VALUES_HEADER = "x-streams-plugin-values";

  protected QueueingConsumer consumer;
  protected final InputHandler handler;
  private final JSONObject originalConfiguration;
  private JSONObject staticConfiguration;

  protected InputConsumer(QueueingConsumer consumer, InputHandler handler, JSONObject config) {
    this.consumer = consumer;
    this.handler = handler;
    this.originalConfiguration = config.getJSONObject("configuration");
    makeStaticConfiguration(config);
  }

  private void makeStaticConfiguration(JSONObject config) {
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

  JSONObject mergeConfigWithHeaders(Map<String, Object> headers) {
    JSONObject result = JSONObject.fromObject(staticConfiguration);
    if (headers != null) {
      JSONObject values = getValuesFromHeader(headers);
      if (values != null) {
        result = interpolateConfig(originalConfiguration, values);
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

  protected static class AMQPMessage implements Message {
    private final Delivery delivery;
    private final Channel channel;

    AMQPMessage(Channel channel, Delivery delivery) {
      this.channel = channel;
      this.delivery = delivery;
    }

    public Map<String, Object> headers() {
      return delivery.getProperties().headers;
    }

    public byte[] body() {
      return delivery.getBody();
    }

    public void ack() throws MessagingException {
      try {
        channel.basicAck(delivery.getEnvelope().getDeliveryTag(), false);
      }
      catch (IOException ioe) {
        throw new MessagingException("Could not ack message", ioe);
      }
    }

    public String routingKey() {
      throw new UnsupportedOperationException("Not supported yet.");
    }
  }

}

