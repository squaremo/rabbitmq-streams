/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.rabbitmq.streams.harness;

import net.sf.json.JSONObject;
import com.rabbitmq.client.QueueingConsumer.Delivery;

/**
 * A callback interface for handling incoming messages.  This interface is for the harness to deliver messages to;
 * the superclass InputReader is for plugins to implement.
 * @see{InputReader}.
 * @author mikeb@lshift.net
 */
public interface InputHandler {

  public void handleDelivery(Delivery delivery, JSONObject config) throws Exception;

}
