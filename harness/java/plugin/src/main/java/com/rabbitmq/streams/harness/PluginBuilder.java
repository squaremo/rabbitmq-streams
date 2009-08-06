package com.rabbitmq.streams.harness;

import com.rabbitmq.client.Channel;
import com.rabbitmq.client.Connection;
import com.rabbitmq.client.impl.ChannelN;
import net.sf.json.JSONNull;
import net.sf.json.JSONObject;

import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.Iterator;
import net.sf.json.JSONArray;

class PluginBuilder {
  private final Logger log;
  private final PluginResourceFactory resources;

  public PluginBuilder(Logger log, PluginResourceFactory resourceFactory) {
    this.log = log;
    resources = resourceFactory;
  }

  // Methods that interpret the JSONObject

  public Plugin buildPlugin(JSONObject configuration) {
    String pluginDirectory = pluginDirectory(configuration);
    try {
      URI libUri = new URI(pluginDirectory + "lib/");
      URLClassLoader ucl = new URLClassLoader(classPathEntries(new URL(pluginDirectory), libUri, jars(libUri)), ClassLoader.getSystemClassLoader());
      Thread.currentThread().setContextClassLoader(ucl);
      String pluginName = configuration.getString("plugin_name");
      JSONObject merged = mergedStaticConfiguration(configuration);
      if (pluginIsServer(configuration)) {
        Server p = constructPlugin(ucl, pluginName);
        configureServer(p, configuration);
        return p;
      }
      else {
        PipelineComponent p = constructPlugin(ucl, pluginName);
        configurePipelineComponent(p, configuration);
        return p;
      }
    }
    catch (Exception ex) {
      log.error(ex);
      return null;
    }
  }

// <editor-fold defaultstate="collapsed" desc="ClassLoader utils">
  private String[] jars(URI libUri) {
    return new File(libUri).list(new FilenameFilter() {

      public boolean accept(File dir, String filename) {
        return filename.endsWith(".jar");
      }
    });
  }

  private JSONObject mergeStaticConfiguration(JSONObject configuration) {
    JSONObject pluginType = configuration.getJSONObject("plugin_type");
    JSONObject staticConfig = configuration.getJSONObject("configuration");
    JSONArray globalConfig = pluginType.getJSONArray("global_configuration_specification");
    JSONObject mergedConfig = new JSONObject();
    for (Object configItem : globalConfig) {
      JSONObject item = (JSONObject) configItem;
      mergedConfig.put(item.getString("name"), JSONObject.fromObject(item.get("value")));
    }
    mergedConfig.putAll(staticConfig);
    return mergedConfig;
  }

  private String pluginDirectory(JSONObject jsonArgs) {
    String pluginDir = "file://" + jsonArgs.getString("plugin_dir");
    if (!pluginDir.endsWith("/")) {
      pluginDir += "/";
    }
    return pluginDir;
  }

  private URL[] classPathEntries(URL pluginUrl, URI libUri, String[] jars) throws MalformedURLException {
    ArrayList<URL> classpathEntries = new ArrayList<URL>();
    classpathEntries.add(pluginUrl);
    if (null != jars) {
      for (String jar : jars) {
        classpathEntries.add(new URL(libUri + jar));
      }
    }
    return classpathEntries.toArray(new URL[classpathEntries.size()]);
  }// </editor-fold>

// <editor-fold defaultstate="expanded" desc="Configuration interpreter">
  
  protected void configureServer(Server plugin, JSONObject configuration) throws Exception {
    connectTerminalsDatabase(plugin, configuration);
    configurePlugin(plugin, configuration);
  }

  protected void configurePipelineComponent(PipelineComponent plugin, JSONObject configuration) throws Exception {
    setStateResourceOnPlugin(plugin, configuration);
    configurePlugin(plugin, configuration);
  }

  protected void configurePlugin(Plugin plugin, JSONObject configuration) throws Exception {
    // FIXME this creates two connections, when we only really need one;
    // we should use the message channel abstraction with the loggers as well.
    Connection messageServerConnection = new AMQPConnectionFactory().connectionFromConfig(configuration.getJSONObject("messageserver"));
    Channel messageServerChannel = messageServerConnection.createChannel();
    setIdForPlugin(plugin, configuration);
    connectDatabaseToPlugin(plugin, configuration);
    Logger iolog = connectLoggerToPlugin(plugin, configuration, messageServerConnection);
    connectNotifierToPlugin(plugin, configuration, messageServerConnection);
    Channel channel = messageServerConnection.createChannel();
    //
    JSONObject staticConfig = mergedStaticConfiguration(configuration);
    MessageResource mr = getMessageResource(pluginIsServer(configuration), staticConfig);
    constructPluginOutputs(configuration, mr);
    constructPluginInputs(configuration, mr);
    plugin.setMessageChannel(mr);
    // And now that everything is in place, let the plugin itself do any setup it so wishes
    plugin.configure(staticConfig);
  }

  protected static JSONObject mergedStaticConfiguration(JSONObject configuration) {
    JSONObject pluginType = configuration.getJSONObject("plugin_type");
    JSONArray globalConfig = pluginType.getJSONArray("global_configuration_specification");
    JSONObject staticConfig = configuration.getJSONObject("configuration");
    JSONObject mergedConfig = new JSONObject();
    for (Object configItem : globalConfig) {
      JSONObject item = (JSONObject) configItem;
      mergedConfig.put(item.getString("name"), JSONObject.fromObject(item.get("value")));
    }
    mergedConfig.putAll(staticConfig);
    return mergedConfig;
  }

  protected void setIdForPlugin(Plugin plugin, JSONObject configuration) {
    String id = idForPlugin(configuration);
    plugin.setId(id);
  }

  protected Logger connectLoggerToPlugin(Plugin plugin, JSONObject configuration, Connection connection) throws IOException {
    AMQPLogger logger = new AMQPLogger((ChannelN) connection.createChannel(), routingKey(configuration));
    Thread logThread = new Thread(logger);
    logThread.setDaemon(true);
    logThread.start();
    logger.info("Harness starting up...");
    plugin.setLog(logger);
    return logger;
  }

  protected void connectNotifierToPlugin(Plugin plugin, JSONObject configuration, Connection connection) throws IOException {
    Notifier notifier = new Notifier((ChannelN) connection.createChannel(), routingKey(configuration));
    Thread thread = new Thread(notifier);
    thread.setDaemon(true);
    thread.start();
    log.info("Harness notifier starting up...");
    plugin.setNotifier(notifier);
  }

  private void constructPluginOutputs(JSONObject configuration, MessageResource channel) {
    JSONObject outputs = configuration.getJSONObject("outputs");
    for (Iterator iterator = outputs.keys(); iterator.hasNext();) {
      String name = (String) iterator.next();
      String exchange = outputs.getString(name);
      channel.declareExchange(name, exchange);
    }
  }

  private void constructPluginInputs(JSONObject configuration, MessageResource channel) {
    JSONObject inputs = configuration.getJSONObject("inputs");
    for (Iterator<String> fields = (Iterator<String>) inputs.keys(); fields.hasNext();) {
      String name = (String) fields.next();
      String queue = inputs.getString(name);
      channel.declareQueue(name, queue);
    }
  }

  private boolean pluginIsServer(JSONObject config) {
    return config.getJSONObject("plugin_type").getString("subtype").equals("server");
  }

  private String idForPlugin(JSONObject config) {
    if (pluginIsServer(config)) {
      return config.getString("server_id");
    }
    else {
      return config.getString("feed_id") + "." + config.getString("node_id");
    }
  }

  private String routingKey(JSONObject config) {
    if (config.containsKey("server_id")) {
      return "." + config.getString("server_id") + "." + config.getString("plugin_name");
    }
    return "." + config.getString("feed_id") + "." + config.getString("plugin_name") + "." + config.getString("node_id");
  }

  protected void setStateResourceOnPlugin(Plugin plugin, JSONObject configuration) throws PluginBuildException {
    plugin.setStateResource(resources.getStateResource(configuration.getString("state")));
  }

  protected void connectDatabaseToPlugin(Plugin plugin, JSONObject configuration) throws Exception {
    if (configuration.has("database") && !JSONNull.getInstance().equals(configuration.get("database"))) {
      String connectionString = configuration.getString("database");
      DatabaseResource db = resources.getDatabase(connectionString);
      log.debug("Database supplied: " + db.getName());
      plugin.setDatabase(db);
    }
  }

  protected void connectTerminalsDatabase(Plugin plugin, JSONObject config) throws Exception {
    String serverId = config.getString("server_id");
    String terminalsDbStr = config.getString("terminals_database");
    log.debug("Terminals database for " + serverId + ": " + terminalsDbStr);
    plugin.setTerminalsDatabase(resources.getDatabase(terminalsDbStr));
  }

  // </editor-fold>
  
  protected void configurationError(String message, Exception e) {
      log.error("Exception thrown while loading & constructing Java plugin: " + message);
      log.error(e);
      // FIXME what here?  Die horribly?
  }

  // Methods that set up the plugin

  protected <C extends Plugin> C constructPlugin(ClassLoader cloader, String pluginName) throws PluginNotFoundException, Exception {
    try {
      C p = null;
      Class<C> clazz = (Class<C>) cloader.loadClass(pluginName);
      p = clazz.getConstructor().newInstance();
      return p;
    } catch (ClassNotFoundException ex) {
      throw new PluginNotFoundException("Cannot find plugin class " + pluginName, ex);
    } catch (NoSuchMethodException ex) {
      throw new PluginNotFoundException("No default constructor for class " + pluginName, ex);
    }
  }

  private MessageResource getMessageResource(boolean server, JSONObject mergedConfig) throws IOException {
    if (server) {
      return resources.getMessageResource(mergedConfig);
    }
    else {
      return resources.getComponentMessageResource(mergedConfig);
    }
  }

  /*public void shutdown() throws IOException {
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
  }*/
  

}

