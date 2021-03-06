package com.rabbitmq.streams.harness;

/**
 *
 * @author mikeb@lshift.net
 */
public interface MessageChannel {
  public void consume(String channelName, InputHandler handler) throws MessagingException;
  public void publish(String channelName, Message msg) throws MessagingException;
}
