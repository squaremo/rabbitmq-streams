/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.rabbitmq.streams.harness;

import com.fourspaces.couchdb.Database;
import com.fourspaces.couchdb.Document;
import com.fourspaces.couchdb.Session;
import com.rabbitmq.client.Channel;
import com.rabbitmq.client.Connection;
import com.rabbitmq.client.QueueingConsumer;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;
import net.sf.json.JSONObject;


/**
 *
 * @author mikeb@lshift.net
 */
class PluginResourceFactory {
  private final Connection connection;
  private final SessionFactory sessionFactory;
  private final Logger log;

  PluginResourceFactory(Connection connection, SessionFactory dbSessionFactory, Logger log) {
    this.connection = connection;
    this.sessionFactory = dbSessionFactory;
    this.log = log;
  }

  protected String stripTrailingSlash(String url) {
        return (url.endsWith("/")) ? url.substring(0, url.length()-1) : url;
  }

  public DatabaseResource getDatabase(String connectionString) throws IOException, PluginBuildException {
    URL url = new URL(stripTrailingSlash(connectionString));
    Session session = sessionFactory.createSession(url.getHost(), url.getPort(), "", "");
    String name = url.getPath().substring(1 + url.getPath().lastIndexOf('/'));
    Database database = session.getDatabase(name);
    if (null==database) throw new PluginBuildException("Database " + url + " does not exist");
    return new CouchDbDatabaseResource(database);
  }

  public MessageResource getComponentMessageResource(JSONObject config) throws IOException {
    return new AMQPComponentMessageChannel(connection.createChannel(), config, log);
  }

  public MessageResource getMessageResource(JSONObject config) throws IOException {
    return new AMQPMessageChannel(connection.createChannel(), config, log);
  }

  public StateResource getStateResource(String stateString) throws PluginBuildException {
    try {
      URL dbURL = new URL(stripTrailingSlash(stateString));
      String path = dbURL.getPath();
      int loc = path.lastIndexOf('/'); // minus document
      String db = path.substring(0, loc);
      int loc2 = db.lastIndexOf('/');
      String dbName = db.substring(loc2);
      Session couchSession = new Session(dbURL.getHost(), dbURL.getPort(), "", "");
      String stateDocName = path.substring(1 + loc);
      Database stateDb = couchSession.getDatabase(dbName);
      return new CouchDbStateResource(stateDb, stateDocName);
    }
    catch (MalformedURLException mue) {
      throw new PluginBuildException("Cannot interpret state document", mue);
    }
  }

}
class CouchDbDatabaseResource implements DatabaseResource {
  private final Database database;

  public CouchDbDatabaseResource(Database db) {
    database = db;
  }

  public JSONObject getDocument(String id) throws IOException {
    Document d = database.getDocument(id);
    return (null == d) ? null : d.getJSONObject();
  }

  public void saveDocument(JSONObject doc, String id) throws IOException {
    Document d = new Document(doc);
    database.saveDocument(d, id);
  }

  public String getName() {
    return database.getName();
  }
}

class AMQPMessageChannel implements MessageResource {
  private final Channel channel;
  private Map<String, AMQPPublisher> outputs;
  private Map<String, String> inputs;
  protected static Map<String, Object> EMPTY_HEADERS = new HashMap(0);
  private final JSONObject config;
  private final Logger log;

  AMQPMessageChannel(Channel channel, JSONObject config, Logger log) {
    this.channel = channel;
    this.config = config;
    this.outputs = new HashMap(1);
    this.inputs = new HashMap(1);
    this.log = log;
  }

  public void declareExchange(String name, String exchange) {
    outputs.put(name, new AMQPPublisher(exchange, channel));
  }

  public void declareQueue(String name, String queue) {
    inputs.put(name, queue);
  }

  public void consume(String channelName, InputHandler handler) throws MessagingException {
    String queue = inputs.get(channelName);
    if (null == queue) {
      throw new MessagingException("No such input " + channelName);
    }
    QueueingConsumer queuer = new QueueingConsumer(channel);
    AMQPInputConsumer consumer = inputConsumer(queuer, handler, config, log);
    Thread consumerThread = new Thread(consumer);
    consumerThread.setDaemon(true);
    consumerThread.start();
    try {
      synchronized(channel) {
        channel.basicConsume(queue, queuer);
      }
    } catch (IOException ex) {
      throw new MessagingException("IOException on consume", ex);
    }
  }


  public void publish(String channelName, Message msg) throws MessagingException {
    AMQPPublisher p = outputs.get(channelName);
    if (null != p) {
      try {
        p.publish(msg);
      } catch (IOException ex) {
        throw new MessagingException("IOException on publish", ex);
      }
    } else {
      throw new MessagingException("No such channel: " + channelName);
    }
  }

  protected AMQPInputConsumer inputConsumer(QueueingConsumer queuer, InputHandler handler, JSONObject config, Logger log) {
    return new DefaultInputConsumer(queuer, handler, config, log);
  }
}

class AMQPComponentMessageChannel extends AMQPMessageChannel {
  AMQPComponentMessageChannel(Channel channel, JSONObject config, Logger log) throws IOException {
    super(channel, config, log);
    synchronized(channel) {
      channel.txSelect();
    }
  }

  private final Object mutex = new Object();
  @Override
  protected AMQPInputConsumer inputConsumer(QueueingConsumer queuer, InputHandler handler, JSONObject config, Logger log) {
    return new SerialisedInputConsumer(queuer, handler, config, log, mutex);
  }
}

class CouchDbStateResource implements StateResource {

  private final Database database;
  private final String docId;

  public CouchDbStateResource(Database db, String id) {
    this.database = db;
    this.docId = id;
  }

  public void setState(Map<String, Object> state) throws IOException {
    Document doc = database.getDocument(docId);
    if (null == doc) {
      doc = new Document();
      doc.setId(docId);
    }
    //doc.clear();
    doc.putAll(state);
    database.saveDocument(doc, docId);
  }

  public Map<String, Object> getState() throws IOException {
    Document doc = database.getDocument(docId);
    if (null == doc) {
      return new JSONObject();
    } else {
      return doc.getJSONObject();
    }
  }
}

// Abstraction of the creation of database sessions; mainly so we can inject this
// for unit testing purposes.
class SessionFactory {

  Session createSession(String host, int port, String user, String passwd) {
    return new Session(host, port, user, passwd);
  }

}
