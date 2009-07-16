package com.rabbitmq.streams.harness;

import com.fourspaces.couchdb.Database;
import com.fourspaces.couchdb.Document;
import com.fourspaces.couchdb.Session;
import com.rabbitmq.client.Connection;
import com.rabbitmq.client.QueueingConsumer;
import com.rabbitmq.client.ShutdownSignalException;
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
import java.util.Map;

public class Harness implements Runnable {

  public Harness(JSONObject configuration) {
    this.configuration = configuration;
    plugin = plugin(configuration);
  }

  /**
   * This constructor is here for the sole purpose of facilitating testing, do not use it in production code!
   * <p/>
   * A message will be written to stdout and stderr so if you use it you may get found out!
   */
  Harness(JSONObject configuration, Plugin plugin) {
    System.err.println("This constructor is for use in unit test code only to facilitate testing - DO NOT USE IN PRODUCTION CODE");
    this.configuration = configuration;
    this.plugin = plugin;
  }

  protected Plugin plugin(JSONObject configuration) {
    String pluginDirectory;
    String pluginName;
    Plugin p = null;

    try {
      pluginDirectory = pluginDirectory(configuration);
      URI libUri = new URI(pluginDirectory + "lib/");

      URLClassLoader ucl = new URLClassLoader(classPathEntries(new URL(pluginDirectory), libUri, jars(libUri)), ClassLoader.getSystemClassLoader());
      Thread.currentThread().setContextClassLoader(ucl);
      pluginName = configuration.getString("plugin_name");
      @SuppressWarnings({"unchecked"}) Class<Plugin> clazz = (Class<Plugin>) ucl.loadClass(pluginName);
      p = clazz.getConstructor().newInstance();
    }
    catch (Exception ex) {
      
    }
    p.setId(idForPlugin(configuration));
    return p;
  }

  protected void configurationError(String message, Exception e) {
      System.err.println("Exception thrown while loading & constructing Java plugin");
      System.err.println(message);
      e.printStackTrace(System.err);
      // FIXME what here?  Die horribly?
  }

  private String[] jars(URI libUri) {
    return new File(libUri).list(new FilenameFilter() {
      public boolean accept(File dir, String filename) {
        return filename.endsWith(".jar");
      }
    });
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
  }

  public void run() {
  }

  public void start() throws IOException {
    messageServerConnection = AMQPConnection.amqConnectionFromConfig(configuration.getJSONObject("messageserver"));
    messageServerChannel = (ChannelN) messageServerConnection.createChannel();
    plugin.setMessageServerChannel(messageServerChannel); // TODO this is needed for the command channel can this be pulled out of server somehow?
    plugin.configure(configuration.getJSONObject("configuration"), configuration.getJSONObject("plugin_type"));
    String id = idForPlugin(configuration);
    setStateResourceOnPlugin(configuration, plugin);
    connectLoggerToPlugin();
    connectDatabaseToPlugin();
    if (pluginIsServer(configuration)) {
      connectTerminalsDatabase(plugin, configuration);
    }

    constructPluginOutputs(configuration.getJSONObject("outputs"));
    constructPluginInputs(configuration.getJSONObject("inputs"));

    new Thread(this).start();
  }

  private void constructPluginOutputs(JSONObject outputs) {
    for (Iterator iterator = outputs.keys(); iterator.hasNext();) {
      String name = (String) iterator.next();
      String exchange = outputs.getString(name);
      plugin.addOutput(name, new Publisher(exchange, messageServerChannel, log));
    }
  }

  @SuppressWarnings("unchecked")
  private void constructPluginInputs(JSONObject inputs) {
    for (Iterator<String> fields = (Iterator<String>) inputs.keys(); fields.hasNext();) {
      final String fieldName = fields.next();
      try {
        final QueueingConsumer consumer = new QueueingConsumer(messageServerChannel);
        final InputReaderRunnable runnable = plugin.handlerRunnable(fieldName);
        runnable.configure(consumer, messageServerChannel, plugin.handler(fieldName), configuration, log);

        new Thread(runnable).start();
        messageServerChannel.basicConsume(inputs.getString(fieldName), false, consumer);
      }
      catch (IOException e) {
        log.fatal("Unable to connect to input " + fieldName);
        System.exit(1);
      }
    }
  }

  private void connectLoggerToPlugin() throws IOException {
    log = new Logger((ChannelN) messageServerConnection.createChannel(), logRoutingKey(configuration));
    Thread logThread = new Thread(log);
    logThread.setDaemon(true);
    logThread.start();
    log.info("Harness starting up...");
    plugin.setLog(log);
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

  private void connectDatabaseToPlugin() throws MalformedURLException {
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

  private void connectTerminalsDatabase(Plugin plugin, JSONObject config) {
    String serverId = config.getString("server_id");
    String terminalsDbStr = config.getString("terminals_database");
    try {
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
    catch (MalformedURLException mue) {
      // FIXME log a fatal error and die horriby
      throw new RuntimeException(mue);
    }
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

  private final Plugin plugin;
  private final JSONObject configuration;
  private Connection messageServerConnection;
  private ChannelN messageServerChannel;
  private Logger log;
}

