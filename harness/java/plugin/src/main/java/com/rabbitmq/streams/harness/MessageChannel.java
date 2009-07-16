/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.rabbitmq.streams.harness;

import java.io.IOException;
import java.util.Map;

/**
 *
 * @author mikeb@lshift.net
 */
public interface MessageChannel {
  public void consume(String channelName, InputHandler handler);
  public void publish(String channelName, byte[] body) throws IOException, MessagingException;
  public void publish(String channelName, byte[] body, Map<String, Object> headers) throws IOException, MessagingException;
  public void publish(String channelName, byte[] body, String routingKey) throws IOException, MessagingException;
  public void publish(String channelName, byte[] body, String routingKey, Map<String, Object> headers) throws IOException, MessagingException;
}
