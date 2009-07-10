package com.rabbitmq.streams.harness;

import com.fourspaces.couchdb.Database;
import com.fourspaces.couchdb.Document;
import com.fourspaces.couchdb.Session;
import net.sf.json.JSONObject;

import java.io.IOException;
import java.net.URL;
import java.util.Map;

/**
 * A superclass for pipeline components. THis does a bit more work than its
 * superclass, Plugin; in particular, in wrapping a transaction around
 * handleDelivery.
 */
public abstract class PipelineComponent extends Plugin {

  final private Object privateLock = new Object();
  final private Database stateDb;
  final private String stateDocName;

  public PipelineComponent(JSONObject config) throws IOException {
    super(config);
    URL dbURL = new URL(config.getString("state"));
    String path = dbURL.getPath();
    int loc = path.lastIndexOf('/'); // minus document
    String db = path.substring(0, loc);
    int loc2 = db.lastIndexOf('/');
    String dbName = db.substring(loc2);

    Session couchSession = new Session(dbURL.getHost(), dbURL.getPort(), "", "");
    stateDocName = path.substring(1 + loc);
    stateDb = couchSession.getDatabase(dbName);
  }

  @Override
  public InputReaderRunnable handlerRunnable(String name) {
    return new TransactionalInputReaderRunnable(privateLock);
  }

  public void publishToChannel(String channel, byte[] body) {
    Publisher publisher = getPublisher(channel);
    if (publisher != null) {
      try {
        publisher.publish(body);
      }
      catch (IOException e) {
        log.error(e);
        e.printStackTrace();
      }
    }
    else {
      log.error("No publisher has been made available to this plugin for the channel named " + channel);
    }
  }

  public void publishToChannel(String channel, byte[] body, Map<String, Object> headers) {
    Publisher publisher = getPublisher(channel);
    if (publisher != null) {
      try {
        publisher.publish(body, headers);
      }
      catch (IOException e) {
        log.error(e);
        e.printStackTrace();
      }
    }
    else {
      log.error("No publisher has been made available to this plugin for the channel named " + channel);
    }
  }

  protected Document getState() throws IOException {
    return stateDb.getDocument(stateDocName);
  }

  protected void setState(Document state) throws IOException {
    stateDb.saveDocument(state, stateDocName);
  }

}
