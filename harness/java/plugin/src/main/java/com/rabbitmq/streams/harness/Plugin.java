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

  protected String id;
  private ChannelN messageServerChannel; // TODO consider encapsulating this and moving to the harness
  private StateResource stateResource;
  protected Logger log;
  protected DatabaseResource privateDb;
  protected DatabaseResource terminalsDatabase;

  protected JSONObject pluginType;
  protected JSONObject staticConfiguration;

  private final Map<String, Publisher> outputs = new HashMap<String, Publisher>();
  private final Map<String, InputHandler> handlers = new HashMap<String, InputHandler>();

  public void configure(final JSONObject staticConfig, final JSONObject pluginType) {
    this.pluginType = pluginType;
    JSONArray globalConfig = pluginType.getJSONArray("global_configuration_specification");
    JSONObject mergedConfig = new JSONObject();
    for (Object configItem : globalConfig) {
      JSONObject item = (JSONObject) configItem;
      mergedConfig.put(item.getString("name"), JSONObject.fromObject(item.get("value")));
    }
    mergedConfig.putAll(staticConfig);
    this.staticConfiguration = mergedConfig;
  }

  protected void setId(String id) {
    this.id = id;
  }

  public String getId() {
    return id;
  }

  public boolean configuredCorrectly()  {
    return true;
  }

  void setStateResource(StateResource state) {
    this.stateResource = state;
  }

  void setTerminalsDatabase(DatabaseResource db) {
    this.terminalsDatabase = db;
  }

  public void addOutput(String channel, Publisher publisher) {
    outputs.put(channel, publisher);
  }

  /**
   * For plugins to get an output channel
   * @param channel the name of the channel required
   */
  public Publisher getPublisher(String channel) {
    return outputs.get(channel);
  }

  /**
   * For plugins to register a handler for handler() to supply to the harness.
   * In other words, this maps a handler to a channel name.
   * @param name
   * @param handler
   */
  protected void registerHandler(String name, InputHandler handler)  {
    handlers.put(name, handler);
  }

  /**
   * For plugins to set their state
   * @param name
   * @return
   */
  protected final void setState(Map<String, Object> state) {
    try {
      stateResource.setState(state);
    }
    catch (IOException ioe) {

    }
  }

  /**
   * For plugins to get their state
   * @return
   */
  protected final Map<String, Object> getState() {
    try {
      return stateResource.getState();
    }
    catch (IOException ioe) {
      log.fatal("Cannot read state");
      this.dieHorribly();
      return null; // obey the type system
    }
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

  public void setDatabase(DatabaseResource database) {
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
