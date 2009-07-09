package com.rabbitmq.streams.harness;

import com.rabbitmq.client.Connection;
import com.rabbitmq.client.ShutdownSignalException;
import com.rabbitmq.client.impl.ChannelN;
import com.fourspaces.couchdb.Database;
import com.fourspaces.couchdb.Session;

import java.io.*;
import java.net.URL;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URLClassLoader;
import java.util.ArrayList;

import net.sf.json.JSONObject;
import net.sf.json.JSONNull;

public class Harness implements Runnable {

  public Harness(JSONObject configuration) {
    this.configuration = configuration;
    plugin = plugin(configuration);
  }

  /**
   * This constructor is here for the sole purpose of facilitating testing, do not use it in production code!
   *
   * A message will be written to stdout and stderr so if you use it you may get found out!
   */
  Harness(JSONObject configuration, Plugin plugin)  {
    System.out.println("This constructor is for use in unit test code only to facilitate testing - DO NOT USE IN PRODUCTION CODE");
    this.configuration = configuration;
    this.plugin = plugin;
    System.err.println("This constructor is for use in unit test code only to facilitate testing - DO NOT USE IN PRODUCTION CODE");
  }

  public Plugin plugin(JSONObject configuration) {
    String pluginDirectory;
    String pluginName;
    Plugin plugin = null;

    try {
      pluginDirectory = pluginDirectory(configuration);
      URI libUri = new URI(pluginDirectory + "lib/");

      URLClassLoader ucl = new URLClassLoader(classPathEntries(new URL(pluginDirectory), libUri, jars(libUri)), ClassLoader.getSystemClassLoader());
      Thread.currentThread().setContextClassLoader(ucl);
      pluginName = configuration.getString("plugin_name");
      @SuppressWarnings({"unchecked"}) Class<Plugin> clazz = (Class<Plugin>) ucl.loadClass(pluginName);
      plugin = clazz.getConstructor(JSONObject.class).newInstance(configuration);
    }
    catch (Exception ex) {
      System.err.println("Exception thrown while loading & constructing Java plugin");
      ex.printStackTrace(System.err);
    }
    return plugin;
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
    // configure message channels required by plugin
    // configure database required by plugin
    // create thread and go!
  }

  public void start() throws IOException {
    connectMessageChannelToPlugin();
    connectLoggerToPlugin();
    connectDatabaseToPlugin();

    plugin.initialise();

    new Thread(this).start();
  }

  private void connectMessageChannelToPlugin() throws IOException {
    messageServerConnection = AMQPConnection.amqConnectionFromConfig(configuration.getJSONObject("messageserver"));
    messageServerChannel = (ChannelN) messageServerConnection.createChannel();
    plugin.setMessageServerChannel(messageServerChannel);
  }

  private void connectLoggerToPlugin() throws IOException {
    log = new Logger((ChannelN) messageServerConnection.createChannel(), logRoutingKey(configuration));
    Thread logThread = new Thread(log);
    logThread.setDaemon(true);
    logThread.start();
    log.info("Harness starting up...");
    plugin.setLog(log);
  }

  private String logRoutingKey(JSONObject config) {
    if (config.containsKey("server_id")) {
      return "." + config.getString("server_id") + "." + config.getString("plugin_name");
    }
    return "." + config.getString("feed_id") + "." + config.getString("plugin_name") + "." + config.getString("node_id");
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
      plugin.setDatabase(database);
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

  private final Plugin plugin;
  private final JSONObject configuration;
  private Connection messageServerConnection;
  private ChannelN messageServerChannel;
  private Logger log;
}

