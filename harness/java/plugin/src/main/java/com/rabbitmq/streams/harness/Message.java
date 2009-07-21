/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.rabbitmq.streams.harness;

import java.util.Map;

/**
 *
 * @author mikeb@lshift.net
 */
public interface Message {
  public abstract Map<String, Object> headers();
  public abstract byte[] body();
  public abstract String routingKey();
}
