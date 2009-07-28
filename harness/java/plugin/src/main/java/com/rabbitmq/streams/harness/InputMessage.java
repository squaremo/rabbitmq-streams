/*
 */

package com.rabbitmq.streams.harness;

import java.util.Map;

/**
 *
 * @author mikeb@lshift.net
 */
public abstract class InputMessage implements Message {
  public abstract InputMessage withHeader(String key, Object val);
  public abstract InputMessage withBody(byte[] body);
  public abstract InputMessage withHeaders(Map<String, Object> headers);
  public abstract void ack() throws MessagingException;

  public InputMessage withBody(String body) {
    return this.withBody(body.getBytes());
  }

  public InputMessage withContentType(String contentType) {
    return this.withHeader("Content-Type", contentType);
  }

}
