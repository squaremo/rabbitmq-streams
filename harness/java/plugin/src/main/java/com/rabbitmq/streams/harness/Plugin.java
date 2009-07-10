package com.rabbitmq.streams.harness;

import com.fourspaces.couchdb.Database;
import com.rabbitmq.client.Connection;
import com.rabbitmq.client.impl.ChannelN;
import net.sf.json.JSONArray;
import net.sf.json.JSONObject;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

public abstract class Plugin {

  public static final String newline = System.getProperty("line.separator");

  public static final String PLUGIN_VALUES_HEADER = "x-streams-plugin-values";

  protected ChannelN messageServerChannel;
  protected Connection messageServerConnection; // TODO does this need to be available to plugins directly
  protected Logger log;
  protected Database privateDb;

  final protected JSONObject pluginType;
  final protected JSONObject config;
  final protected JSONObject uninterpolatedConfiguration;
  final protected JSONObject staticConfiguration;

  private final Map<String, Publisher> outputs = new HashMap<String, Publisher>();
  private final Map<String, InputHandler> handlers = new HashMap<String, InputHandler>();

  public Plugin(final JSONObject config) throws IOException {
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
    this.uninterpolatedConfiguration = mergedConfig;
    this.staticConfiguration = interpolateConfig(mergedConfig, new HashMap<String, Object>());
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

  public void registerHandler(String name, InputHandler handler)  {
    handlers.put(name, handler);
  }

  public InputHandler handler(String name)  {
    return handlers.get(name);
  }

  public abstract InputReaderRunnable handlerRunnable(String name); 

  public void setMessageServerChannel(ChannelN channelN) {
    messageServerChannel = channelN;
  }

  public void setMessageServerConnection(Connection connection) {
    messageServerConnection = connection;
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

  /**
   * Set values in the header.
   */
  protected static void setValuesInHeader(Map<String, Object> headersToMutate, JSONObject vals) {
    headersToMutate.put(Plugin.PLUGIN_VALUES_HEADER, vals);
  }

  protected static JSONObject getValuesFromHeader(Map<String, Object> headers) {
    return (headers.containsKey(PLUGIN_VALUES_HEADER)) ? JSONObject.fromObject(headers.get(PLUGIN_VALUES_HEADER)) : null;
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

  @SuppressWarnings({"unchecked"})
  protected final JSONObject configForHeaders(Map<String, Object> headers) throws Exception {
    if (headers == null) {
      return this.staticConfiguration;
    }
    JSONObject conf = getValuesFromHeader(headers);
    if (conf != null) {
      log.debug("Plugin values found in header: " + conf.toString());
      return interpolateConfig(this.uninterpolatedConfiguration, conf);
    }
    else {
      return this.staticConfiguration;
    }
  }

}
