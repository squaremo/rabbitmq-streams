package com.rabbitmq.streams.harness;

/**
 * A superclass for pipeline components. THis does a bit more work than its
 * superclass, Plugin; in particular, in wrapping a transaction around
 * handleDelivery.
 */
public abstract class PipelineComponent extends Plugin {

  protected void registerInput(String channel, InputReader reader) throws PluginBuildException {
    try {
      this.messageChannel.consume(channel, reader);
    } catch (MessagingException ex) {
      throw new PluginBuildException("Unable to register input", ex);
    }
  }

  public void publishToChannel(String channel, InputMessage msg) throws MessagingException {
    this.messageChannel.publish(channel, msg);
  }

}
