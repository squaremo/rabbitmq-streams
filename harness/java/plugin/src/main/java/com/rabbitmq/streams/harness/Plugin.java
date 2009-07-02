package com.rabbitmq.streams.harness;

import java.io.IOException;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.URL;
import java.util.Iterator;
import java.util.Map;

import net.sf.json.JSONArray;
import net.sf.json.JSONNull;
import net.sf.json.JSONObject;

import com.fourspaces.couchdb.Database;
import com.fourspaces.couchdb.Session;
import com.rabbitmq.client.Connection;
import com.rabbitmq.client.QueueingConsumer;
import com.rabbitmq.client.QueueingConsumer.Delivery;
import com.rabbitmq.client.ShutdownSignalException;
import com.rabbitmq.client.AMQP.BasicProperties;
import com.rabbitmq.client.impl.ChannelN;

public abstract class Plugin implements Runnable {

  public static final String newline = System.getProperty("line.separator");

  public static final String PLUGIN_CONFIG_HEADER = "x-streams-plugin-config";
  
  static final BasicProperties basicPropsPersistent = new BasicProperties();
  static {
    basicPropsPersistent.deliveryMode = 2;
  }

  static final BasicProperties propertiesWithHeaders(Map<String, Object> headers) {
    BasicProperties props = new BasicProperties();
    props.deliveryMode = 2;
    props.headers.putAll(headers);
    return props;
  }

  final protected Connection messageServerConnection;
  final protected ChannelN messageServerChannel;
  final protected JSONObject pluginType;
  final protected JSONObject config;
  final protected JSONObject staticConfiguration;
  final protected Database privateDb;
  final protected Logger log;

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
    this.staticConfiguration = mergedConfig;

    JSONObject messageServerSpec = config.getJSONObject("messageserver");
    messageServerConnection = AMQPConnection.amqConnectionFromConfig(messageServerSpec);
    messageServerChannel = (ChannelN) messageServerConnection.createChannel();
    ChannelN logChannel = (ChannelN) messageServerConnection.createChannel();

    String logRoutingKey = logRoutingKey(config);

    log = new Logger(logChannel, logRoutingKey);
    new Thread(log).start();
    log.info("Starting up...");

    Database privDb = null;
    if (config.has("database") && !JSONNull.getInstance().equals(config.get("database"))) {
      String privDbStr = config.getString("database");
      URL privDbURL = new URL(privDbStr);
      Session privDbCouchSession = new Session(privDbURL.getHost(), privDbURL.getPort(), "", "");
      String privDbPath = privDbURL.getPath();
      int loc = privDbPath.lastIndexOf('/');
      String privDbName = privDbPath.substring(1 + loc);
      // We do this in two steps, since if the DB already exists, couchdb4j will
      // get a 412 (precondition failed) and return null.
      privDbCouchSession.createDatabase(privDbName);
      privDb = privDbCouchSession.getDatabase(privDbName);
      log.debug("Database supplied: " + privDb.getName());
    }
    privateDb = privDb;
  }

  private String logRoutingKey(JSONObject config) {
    if (config.containsKey("server_id")) {
      return "." + config.getString("server_id") + "." + config.getString("plugin_name");
    }
    return "." + config.getString("feed_id") + "." + config.getString("plugin_name") + "." + config.getString("node_id");
  }

  protected abstract Publisher publisher(String name, String exchange);

  protected abstract Runnable inputReaderRunnable(Getter get,
    QueueingConsumer consumer);

  protected final void dieHorribly() {
    System.exit(1);
  }

  private void noSuchField(String name) {
    log.fatal("No such field: " + name);
    dieHorribly();
  }

  private void illegalAccess(String name) {
    log.fatal("Illegal access: " + name);
    dieHorribly();
  }

  protected final JSONObject configForDelivery(Delivery delivery) throws Exception {
    Map<String, Object> headers = delivery.getProperties().headers;
    if (headers != null && headers.containsKey(PLUGIN_CONFIG_HEADER)) {
      String confStr = headers.get(PLUGIN_CONFIG_HEADER).toString();
      log.debug("Plugin config found: " + confStr);
      JSONObject headerConf = (JSONObject) JSONObject.fromObject(confStr);
      JSONObject dynamicConf = JSONObject.fromObject(this.staticConfiguration);
      dynamicConf.putAll(headerConf);
      return dynamicConf;
    }
    else {
      return this.staticConfiguration;
    }
  }

  @SuppressWarnings("unchecked")
  protected void constructInputs(JSONObject inputs) {
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
  protected void constructOutputs(JSONObject outputs) {
    for (Iterator<String> outKeysIt = (Iterator<String>) outputs.keys(); outKeysIt
           .hasNext();) {
      String name = (String) outKeysIt.next();
      String exchange = outputs.getString(name);
      try {
        Publisher p = publisher(name, exchange);
        Setter setter = this.outputSetter(name, p);
        setter.set(p);
      } catch (IllegalAccessException iac) {
        illegalAccess(name);
      }
    }
  }
  
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
          } catch (IllegalArgumentException e) {
            Plugin.this.log.fatal(e);
            dieHorribly();
            return;
          } catch (IllegalAccessException e) {
            Plugin.this.log.fatal(e);
            dieHorribly();
            return;
          }
        }
      };
    } catch (NoSuchFieldException nsfe) {
      try {
        final Method pluginQueueMethod = getClass().getMethod(name, p.getClass());
        return new Setter() {
          public void set(Publisher pub) {
            try {
              pluginQueueMethod.invoke(Plugin.this, new Object[] {pub});
            } catch (IllegalArgumentException e) {
              Plugin.this.log.fatal(e);
              dieHorribly();
              return;
            } catch (IllegalAccessException e) {
              Plugin.this.log.fatal(e);
              dieHorribly();
              return;
            } catch (InvocationTargetException e) {
              Plugin.this.log.fatal(e);
              dieHorribly();
              return;
            }
          }
        };
      } catch (NoSuchMethodException nsme) {
        noSuchField(name);
        return null;
      }
    } catch (SecurityException se) {
      log.fatal(se);
      dieHorribly();
      return null; // .. but die will have exited
    }
  }
  
  protected void postConstructorInit() throws IllegalArgumentException, SecurityException {

    // set up outputs FIRST, so we don't start processing messages
    // before we can put them anywhere
    JSONObject outputs = config.getJSONObject("outputs");
    constructOutputs(outputs);

    JSONObject inputs = config.getJSONObject("inputs");
    constructInputs(inputs);
  }

  public void run() {
  }

  public void shutdown() throws IOException {
    if (messageServerChannel.isOpen()) {
      try {
        messageServerChannel.close();
      }
      catch (ShutdownSignalException ignored) {
      }
    }
    log.shutdown();
    if (messageServerConnection.isOpen()) {
      try {
        messageServerConnection.close();
      }
      catch (ShutdownSignalException ignored) {
      }
    }
  }

  public final void start() throws Exception {
    new Thread(this).start();
  }

}
