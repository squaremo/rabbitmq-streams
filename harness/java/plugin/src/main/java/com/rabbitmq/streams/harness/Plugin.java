package com.rabbitmq.streams.harness;

import com.fourspaces.couchdb.Database;
import com.rabbitmq.client.QueueingConsumer.Delivery;
import com.rabbitmq.client.impl.ChannelN;
import net.sf.json.JSONArray;
import net.sf.json.JSONObject;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

public abstract class Plugin {

  public static final String newline = System.getProperty("line.separator");

  private ChannelN messageServerChannel; // TODO consider encapsulating this and moving to the harness
  protected Logger log;
  protected Database privateDb;

  final protected JSONObject pluginType;
  final protected JSONObject config;

  private final Map<String, Publisher> outputs = new HashMap<String, Publisher>();
  private final Map<String, InputHandler> handlers = new HashMap<String, InputHandler>();

  public Plugin(final JSONObject config) {
    this.config = config;
    pluginType = config.getJSONObject("plugin_type");
    JSONArray globalConfig = pluginType.getJSONArray("global_configuration_specification");
    JSONObject mergedConfig = new JSONObject();
    for (Object configItem : globalConfig) {
      JSONObject item = (JSONObject) configItem;
      mergedConfig.put(item.getString("name"), JSONObject.fromObject(item.get("value")));
    }
    JSONObject userConfig = config.getJSONObject("configuration");
    mergedConfig.putAll(userConfig);
  }

  public boolean configuredCorrectly()  {
    return true;
  }

  public void addOutput(String channel, Publisher publisher) {
    outputs.put(channel, publisher);
  }

  public Publisher getPublisher(String channel) {
    return outputs.get(channel);
  }

  /**
   * For plugins to register a handler for handler() to suplpy to the harness.
   * In other words, this maps a handler to a channel name.
   * @param name
   * @param handler
   */
  public void registerHandler(String name, InputHandler handler)  {
    handlers.put(name, handler);
  }

  InputHandler handler(String name)  {
    return handlers.get(name);
  }

  public abstract InputReaderRunnable handlerRunnable(String name); 

  public void setMessageServerChannel(ChannelN channelN) {
    messageServerChannel = channelN;
  }

  public void setLog(Logger log) {
    this.log = log;
  }

  public void setDatabase(Database database) {
    privateDb = database;
  }

  protected final void dieHorribly() {
    System.exit(1);
  }

  protected final void ack(Delivery delivery) throws IOException {
    this.ack(delivery.getEnvelope().getDeliveryTag());
  }

  protected final void ack(long tag) throws IOException {
    messageServerChannel.basicAck(tag, false);
  }

}
