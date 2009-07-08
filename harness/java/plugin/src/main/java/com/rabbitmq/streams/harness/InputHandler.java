/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.rabbitmq.streams.harness;

import net.sf.json.JSONObject;
import com.rabbitmq.client.QueueingConsumer.Delivery;

/**
 * A callback interface for handling incoming messages.  This one has
 * the full interface; most plugins will in fact want to use a variation on
 * @see{InputReader}.
 * @author mikeb@lshift.net
 */
public interface InputHandler {

    public void handleDelivery(Delivery delivery, JSONObject config) throws Exception;

}
