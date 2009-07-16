package com.rabbitmq.streams.harness;

import java.io.IOException;
import java.util.Map;

/**
 * A superclass for pipeline components. THis does a bit more work than its
 * superclass, Plugin; in particular, in wrapping a transaction around
 * handleDelivery.
 */
public abstract class PipelineComponent extends Plugin {

  final private Object privateLock = new Object();

  protected void registerInput(String channel, InputReader reader) {
    // TODO: Make this look like a state monad
    this.messageChannel.consume(channel, reader);
  }

  protected void publishToChannel(String channel, byte[] body) throws IOException, MessagingException {
    this.messageChannel.publish(channel, body);
  }

  public void publishToChannel(String channel, byte[] body, Map<String, Object> headers) throws IOException, MessagingException {
    this.messageChannel.publish(channel, body, headers);
  }

}
