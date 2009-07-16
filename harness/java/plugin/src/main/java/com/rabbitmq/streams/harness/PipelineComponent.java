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

}
