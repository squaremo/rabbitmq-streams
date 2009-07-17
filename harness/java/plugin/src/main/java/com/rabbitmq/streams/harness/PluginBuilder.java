package com.rabbitmq.streams.harness;

import com.fourspaces.couchdb.Database;
import com.fourspaces.couchdb.Document;
import com.fourspaces.couchdb.Session;
import com.rabbitmq.client.Channel;
import com.rabbitmq.client.Connection;
import com.rabbitmq.client.QueueingConsumer;
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
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import net.sf.json.JSONArray;

public class PluginBuilder {
  private final Logger log;

  public PluginBuilder(Logger log) {
    this.log = log;
  }

  // Methods that interpret the JSONObject

  public Plugin buildPlugin(JSONObject configuration) {
    String pluginDirectory = pluginDirectory(configuration);
    try {
      URI libUri = new URI(pluginDirectory + "lib/");
      URLClassLoader ucl = new URLClassLoader(classPathEntries(new URL(pluginDirectory), libUri, jars(libUri)), ClassLoader.getSystemClassLoader());
      Thread.currentThread().setContextClassLoader(ucl);
      String pluginName = configuration.getString("plugin_name");
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
  protected void configurePlugin(Plugin plugin, JSONObject configuration) throws Exception {
    Connection messageServerConnection = AMQPConnection.amqConnectionFromConfig(configuration.getJSONObject("messageserver"));
    Channel messageServerChannel = messageServerConnection.createChannel();
    plugin.configure(mergedStaticConfiguration(configuration));
    String id = idForPlugin(configuration);
    setStateResourceOnPlugin(configuration, plugin);
    if (configuration.containsKey("database")) {
      connectDatabaseToPlugin(plugin, configuration);
    }
    Logger iolog = connectLoggerToPlugin(plugin, configuration, messageServerConnection);
    Channel channel = messageServerConnection.createChannel();
    AMQPMessageChannel c = new AMQPMessageChannel(channel, configuration.getJSONObject("configuration"));
    constructPluginOutputs(configuration, c);
    constructPluginInputs(configuration, c);
    plugin.setMessageChannel(c);
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

  private Logger connectLoggerToPlugin(Plugin plugin, JSONObject configuration, Connection connection) throws IOException {
    AMQPLogger log = new AMQPLogger((ChannelN) connection.createChannel(), logRoutingKey(configuration));
    Thread logThread = new Thread(log);
    logThread.setDaemon(true);
    logThread.start();
    log.info("Harness starting up...");
    plugin.setLog(log);
    return log;
  }

  protected void configureServer(Plugin plugin, JSONObject configuration) throws Exception {
    configurePlugin(plugin, configuration);
    connectTerminalsDatabase(plugin, configuration);
  }

  protected void configurePipelineComponent(Plugin plugin, JSONObject configuration) throws Exception {
    configurePlugin(plugin, configuration);
  }

  private void constructPluginOutputs(JSONObject configuration, AMQPMessageChannel channel) {
    JSONObject outputs = configuration.getJSONObject("outputs");
    for (Iterator iterator = outputs.keys(); iterator.hasNext();) {
      String name = (String) iterator.next();
      String exchange = outputs.getString(name);
      channel.declareExchange(name, exchange);
    }
  }

  @SuppressWarnings("unchecked")
  private void constructPluginInputs(JSONObject configuration, AMQPMessageChannel channel) {
    JSONObject inputs = configuration.getJSONObject("inputs");
    for (Iterator<String> fields = (Iterator<String>) inputs.keys(); fields.hasNext();) {
      String name = (String) fields.next();
      String queue = inputs.getString(name);
      channel.declareQueue(name, queue);
    }
  }

  private boolean pluginIsServer(JSONObject config) {
    return config.getJSONObject("plugin_type").getString("type").equals("server");
  }

  private String idForPlugin(JSONObject config) {
    if (pluginIsServer(config)) {
      return config.getString("server_id");
    }
    else {
      return config.getString("feed_id") + config.getString("node_id");
    }
  }

  private String logRoutingKey(JSONObject config) {
    if (pluginIsServer(config)) {
      return "." + config.getString("server_id") + "." + config.getString("plugin_name");
    }
    return "." + config.getString("feed_id") + "." + config.getString("plugin_name") + "." + config.getString("node_id");
  }

  protected void setStateResourceOnPlugin(JSONObject configuration, Plugin plugin) {
    String url = configuration.getString("state");
    try {
      URL dbURL = new URL(url);
      String path = dbURL.getPath();
      int loc = path.lastIndexOf('/'); // minus document
      String db = path.substring(0, loc);
      int loc2 = db.lastIndexOf('/');
      String dbName = db.substring(loc2);
      Session couchSession = new Session(dbURL.getHost(), dbURL.getPort(), "", "");
      String stateDocName = path.substring(1 + loc);
      Database stateDb = couchSession.getDatabase(dbName);
      plugin.setStateResource(new CouchDbStateResource(stateDb, stateDocName));
    }
    catch (MalformedURLException mue) {
      configurationError("Error constructing state resource", mue);
    }
  }

  private void connectDatabaseToPlugin(Plugin plugin, JSONObject configuration) throws Exception {
    if (configuration.has("database") && !JSONNull.getInstance().equals(configuration.get("database"))) {
      String connectionString = configuration.getString("database");
      URL url = new URL(connectionString);
      Session session = new Session(url.getHost(), url.getPort(), "", "");
      String name = url.getPath().substring(1 + url.getPath().lastIndexOf('/'));
      // We do this in two steps, since if the DB already exists, couchdb4j will get a 412 (precondition failed) and return null.
      session.createDatabase(name);
      Database database = session.getDatabase(name);
      log.debug("Database supplied: " + database.getName());
      plugin.setDatabase(new CouchDbDatabaseResource(database));
    }
  }

  private void connectTerminalsDatabase(Plugin plugin, JSONObject config) throws Exception {
    String serverId = config.getString("server_id");
    String terminalsDbStr = config.getString("terminals_database");
    URL terminalsDbUrl = new URL(terminalsDbStr);
    Session couchSession = new Session(terminalsDbUrl.getHost(), terminalsDbUrl.getPort(), "", "");
    String path = terminalsDbUrl.getPath();
    int loc;
    if (path.endsWith("/")) {
      loc = path.substring(0, path.length() - 1).lastIndexOf('/');
    }
    else {
      loc = path.lastIndexOf('/');
    }
    String terminalsDbName = path.substring(loc);
    Database terminalsDatabase = couchSession.getDatabase(terminalsDbName);
    plugin.setTerminalsDatabase(new CouchDbDatabaseResource(terminalsDatabase));
  }

  // </editor-fold>
  
  protected void configurationError(String message, Exception e) {
      log.error("Exception thrown while loading & constructing Java plugin: " + message);
      log.error(e);
      // FIXME what here?  Die horribly?
  }

  // Methods that set up the plugin

  protected <C extends Plugin> C constructPlugin(ClassLoader cloader, String pluginName) throws Exception {
    C p = null;
    Class<C> clazz = (Class<C>) cloader.loadClass(pluginName);
    p = clazz.getConstructor().newInstance();
    return p;
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
  
  protected static class CouchDbStateResource implements StateResource {
    private final Database database;
    private final String docId;

    public CouchDbStateResource(Database db, String id) {
      this.database = db;
      this.docId = id;
    }

    public void setState(Map<String, Object> state) throws IOException {
      Document doc = database.getDocument(docId);
      if (null==doc) {
        doc = new Document();
        doc.setId(docId);
      }
      doc.clear();
      doc.putAll(state);
      database.saveDocument(doc, docId);
    }

    public Map<String, Object> getState() throws IOException {
      Document doc = database.getDocument(docId);
      if (null==doc) {
        return new JSONObject();
      }
      else {
        return doc.getJSONObject();
      }
    }
  }

  protected static class CouchDbDatabaseResource implements DatabaseResource {
    private final Database database;

    public CouchDbDatabaseResource(Database db) {
      database = db;
    }

    public JSONObject getDocument(String id) throws IOException {
      return database.getDocument(id).getJSONObject();
    }
  }

  protected static class AMQPMessageChannel implements MessageChannel {
    private final Channel channel;
    private Map<String, AMQPPublisher> outputs;
    private Map<String, String> inputs;
    protected static Map<String, Object> EMPTY_HEADERS = new HashMap(0);
    private final JSONObject config;

    AMQPMessageChannel(Channel channel, JSONObject config) {
      this.channel = channel;
      this.config = config;
      this.outputs = new HashMap(1);
      this.inputs = new HashMap(1);
    }

    void declareExchange(String name, String exchange) {
      outputs.put(name, new AMQPPublisher(exchange, channel));
    }

    void declareQueue(String name, String queue) {
      inputs.put(name, queue);
    }

    public void consume(String channelName, InputHandler handler) {
      QueueingConsumer queuer = new QueueingConsumer(channel);
      AMQPInputConsumer consumer = new DefaultInputConsumer(queuer, handler, config);
      new Thread(consumer).start();
    }

    public void publish(String channelName, Message msg) throws IOException, MessagingException {
      AMQPPublisher p = outputs.get(channelName);
      if (null!=p) {
        p.publish(msg);
      }
      else {
        throw new MessagingException("No such channel: " + channelName);
      }
    }


  }

}

