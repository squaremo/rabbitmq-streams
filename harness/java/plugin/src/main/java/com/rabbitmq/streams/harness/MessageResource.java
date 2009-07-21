/*
 */

package com.rabbitmq.streams.harness;

/**
 *
 * @author mikeb@lshift.net
 */
public interface MessageResource extends MessageChannel {

  public void declareExchange(String name, String exchange);
  public void declareQueue(String name, String queue);
  
}
