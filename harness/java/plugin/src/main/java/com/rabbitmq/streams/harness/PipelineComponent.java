package com.rabbitmq.streams.harness;

import java.io.IOException;

/**
 * A superclass for pipeline components. THis does a bit more work than its
 * superclass, Plugin; in particular, in wrapping a transaction around
 * handleDelivery.
 */
public abstract class PipelineComponent extends Plugin {

  protected void registerInput(String channel, InputReader reader) {
    // TODO: Make this look like a state monad
    this.messageChannel.consume(channel, reader);
  }

  public void publishToChannel(String channel, InputMessage msg) throws IOException, MessagingException {
    this.messageChannel.publish(channel, msg);
  }

}
