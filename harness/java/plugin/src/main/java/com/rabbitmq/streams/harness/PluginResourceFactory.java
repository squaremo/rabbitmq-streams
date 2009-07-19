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
 * @author mikeb
 */
public class PluginResourceFactory {
  private final Connection connection;

  public PluginResourceFactory(Connection connection) {
    this.connection = connection;
  }

  public DatabaseResource getDatabase(String connectionString) throws IOException {
    URL url = new URL(connectionString);
      Session session = new Session(url.getHost(), url.getPort(), "", "");
      String name = url.getPath().substring(1 + url.getPath().lastIndexOf('/'));
      // We do this in two steps, since if the DB already exists, couchdb4j will get a 412 (precondition failed) and return null.
      session.createDatabase(name);
      Database database = session.getDatabase(name);
      return new CouchDbDatabaseResource(database);
  }

  public MessageResource getMessageResource(JSONObject config) throws IOException {
    return new AMQPMessageChannel(connection.createChannel(), config);
  }

  public StateResource getStateResource(String stateString) throws PluginBuildException {
    try {
      URL dbURL = new URL(stateString);
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

  protected static class CouchDbDatabaseResource implements DatabaseResource {
    private final Database database;

    public CouchDbDatabaseResource(Database db) {
      database = db;
    }

    public JSONObject getDocument(String id) throws IOException {
      return database.getDocument(id).getJSONObject();
    }

    public void saveDocument(JSONObject doc, String id) throws IOException {
      Document d = new Document(doc);
      database.saveDocument(d, id);
    }

    public String getName() {
      return database.getName();
    }
  }

  protected static class AMQPMessageChannel implements MessageResource {
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

    public void declareExchange(String name, String exchange) {
      outputs.put(name, new AMQPPublisher(exchange, channel));
    }

    public void declareQueue(String name, String queue) {
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

}
