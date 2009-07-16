/*
 */

package com.rabbitmq.streams.harness;

import java.util.Map;

/**
 *
 * @author mikeb@lshift.net
 */
public interface Message {
  public Map<String, Object> headers();
  public byte[] body();
  public void ack() throws MessagingException;
  public String routingKey();
}
