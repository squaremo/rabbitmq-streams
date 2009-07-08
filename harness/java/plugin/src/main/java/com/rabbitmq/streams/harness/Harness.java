package com.rabbitmq.streams.harness;

import com.rabbitmq.client.Connection;
import com.rabbitmq.client.ShutdownSignalException;
import com.rabbitmq.client.impl.ChannelN;
import com.fourspaces.couchdb.Database;
import com.fourspaces.couchdb.Session;

import java.io.IOException;
import java.net.URL;
import java.net.MalformedURLException;

import net.sf.json.JSONObject;
import net.sf.json.JSONNull;

public class Harness implements Runnable {

  public Harness(Plugin plugin) {
    this.plugin = plugin;
    configuration = plugin.getConfiguration();
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

