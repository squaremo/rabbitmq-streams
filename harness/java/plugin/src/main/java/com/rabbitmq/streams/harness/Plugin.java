package com.rabbitmq.streams.harness;

import com.fourspaces.couchdb.Database;
//import com.rabbitmq.client.AMQP.BasicProperties;
import com.rabbitmq.client.QueueingConsumer;
import com.rabbitmq.client.Connection;
import com.rabbitmq.client.QueueingConsumer.Delivery;
import com.rabbitmq.client.impl.ChannelN;
import net.sf.json.JSONArray;
import net.sf.json.JSONObject;

import java.io.IOException;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Iterator;
import java.util.Map;
import java.util.HashMap;

public abstract class Plugin implements Runnable {

  public static final String newline = System.getProperty("line.separator");

  public static final String PLUGIN_VALUES_HEADER = "x-streams-plugin-values";

  // FIXME: when we can clone properties, it'd be good to use the constants
  // in MessageProperties here and below.
//  static final BasicProperties basicPropsPersistent = new BasicProperties();
//
//  static {
//    basicPropsPersistent.deliveryMode = 2;
//  }
//
//  static BasicProperties propertiesWithHeaders(Map<String, Object> headers) {
//    BasicProperties props = new BasicProperties();
//    props.deliveryMode = 2;
//    props.headers = headers;
//    return props;
//  }

  protected ChannelN messageServerChannel;
  protected Connection messageServerConnection; // TODO does this need to be available to plugins directly
  protected Logger log;
  protected Database privateDb;

  final protected JSONObject pluginType;
  final protected JSONObject config;
  final protected JSONObject uninterpolatedConfiguration;
  final protected JSONObject staticConfiguration;

  private final Map<String, Publisher> outputs = new HashMap<String, Publisher>();

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

  public void addOutput(String channel, Publisher publisher)  {
    log.debug("Adding publisher for channel - " + channel);
    outputs.put(channel, publisher);
  }

  public Publisher getPublisher(String channel) {
    log.debug("Obtaining publisher for channel - " + channel + " - " + outputs.get(channel));
    return outputs.get(channel);
  }


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

//  protected abstract Publisher publisher(String name, String exchange);

  protected abstract Runnable inputReaderRunnable(Getter get, QueueingConsumer consumer);

  protected final void dieHorribly() {
    System.exit(1);
  }

  private void noSuchField(String name) {
    log.fatal("No such field: " + name);
    dieHorribly();
  }

//  private void illegalAccess(String name) {
//    log.fatal("Illegal access: " + name);
//    dieHorribly();
//  }

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

  protected final JSONObject configForHeaders(Map<String, Object> headers) throws Exception {
    if (headers ==null) return this.staticConfiguration;
    JSONObject conf = getValuesFromHeader(headers);
    if (conf != null) {
      log.debug("Plugin values found in header: " + conf.toString());
      return interpolateConfig(this.uninterpolatedConfiguration, conf);
    }
    else {
      return this.staticConfiguration;
    }
  }

  @SuppressWarnings("unchecked")
  protected void constructInputs(JSONObject inputs) {
    // TODO refactor this to avoid reflection
    for (Iterator<String> inKeysIt = (Iterator<String>) inputs.keys(); inKeysIt.hasNext();) {
      final String fieldName = inKeysIt.next();
      try {
        final QueueingConsumer consumer = new QueueingConsumer(messageServerChannel);
        new Thread(inputReaderRunnable(inputGetter(fieldName), consumer)).start();
        messageServerChannel.basicConsume(inputs.getString(fieldName), false, consumer);
      }
      catch (IOException ioe) {
        log.fatal("IOException connecting to input " + fieldName);
        dieHorribly();
      }
    }
  }

  @SuppressWarnings("unchecked")
//  protected void constructOutputs(JSONObject outputs) {
//    // TODO refactor this to avoid reflection
//    for (Iterator<String> outKeysIt = (Iterator<String>) outputs.keys(); outKeysIt.hasNext();) {
//      String name = outKeysIt.next();
//      String exchange = outputs.getString(name);
//      try {
//        Publisher p = publisher(name, exchange);
//        Setter setter = this.outputSetter(name, p);
//        setter.set(p);
//      }
//      catch (IllegalAccessException iac) {
//        illegalAccess(name);
//      }
//    }
//  }

  static interface Getter {
    InputReader get() throws IllegalAccessException;
  }

  static interface Setter {
    void set(Publisher pub) throws IllegalAccessException;
  }

  protected final Getter inputGetter(String name) {
    try {
      final Field pluginQueueField = getClass().getField(name);
      return new Getter() {
        public InputReader get() {
          try {
            return (InputReader) pluginQueueField.get(Plugin.this);
          }
          catch (IllegalArgumentException e) {
            Plugin.this.log.fatal(e);
            dieHorribly();
            return null;
          }
          catch (IllegalAccessException e) {
            Plugin.this.log.fatal(e);
            dieHorribly();
            return null;
          }
        }
      };
    }
    catch (NoSuchFieldException nsfe) {
      try {
        final Method pluginQueueMethod = getClass().getMethod(name);
        return new Getter() {
          public InputReader get() {
            try {
              return (InputReader) pluginQueueMethod.invoke(Plugin.this);
            }
            catch (IllegalArgumentException e) {
              Plugin.this.log.fatal(e);
              dieHorribly();
              return null;
            }
            catch (IllegalAccessException e) {
              Plugin.this.log.fatal(e);
              dieHorribly();
              return null;
            }
            catch (InvocationTargetException e) {
              Plugin.this.log.fatal(e);
              dieHorribly();
              return null;
            }
          }
        };
      }
      catch (NoSuchMethodException nsme) {
        noSuchField(name);
        return null;
      }
    }
    catch (SecurityException se) {
      log.fatal(se);
      dieHorribly();
      return null; // .. but die will have exited
    }
  }

  protected final Setter outputSetter(String name, Publisher p) {
    try {
      final Field pluginQueueField = getClass().getField(name);
      return new Setter() {
        public void set(Publisher pub) {
          try {
            pluginQueueField.set(Plugin.this, pub);
          }
          catch (IllegalArgumentException e) {
            Plugin.this.log.fatal(e);
            dieHorribly();
          }
          catch (IllegalAccessException e) {
            Plugin.this.log.fatal(e);
            dieHorribly();
          }
        }
      };
    }
    catch (NoSuchFieldException nsfe) {
      try {
        final Method pluginQueueMethod = getClass().getMethod(name, p.getClass());
        return new Setter() {
          public void set(Publisher pub) {
            try {
              pluginQueueMethod.invoke(Plugin.this, pub);
            }
            catch (IllegalArgumentException e) {
              Plugin.this.log.fatal(e);
              dieHorribly();
            }
            catch (IllegalAccessException e) {
              Plugin.this.log.fatal(e);
              dieHorribly();
            }
            catch (InvocationTargetException e) {
              Plugin.this.log.fatal(e);
              dieHorribly();
            }
          }
        };
      }
      catch (NoSuchMethodException nsme) {
        noSuchField(name);
        return null;
      }
    }
    catch (SecurityException se) {
      log.fatal(se);
      dieHorribly();
      return null; // .. but die will have exited
    }
  }

  public void initialise() throws IllegalArgumentException, SecurityException {
    // set up outputs FIRST, so we don't start processing messages before we can put them anywhere
//    JSONObject outputs = config.getJSONObject("outputs");
//    constructOutputs(outputs);

    JSONObject inputs = config.getJSONObject("inputs");
    constructInputs(inputs);
  }

  protected void postConstructorInit() throws IllegalArgumentException, SecurityException {
    // TODO remove as part of refactor
  }

  public void run() {
  }

  public void shutdown() throws IOException {
  }

  public final void start() throws Exception {
  }

}
