package com.rabbitmq.streams.harness;

import net.sf.json.JSONObject;
import com.rabbitmq.client.QueueingConsumer.Delivery;

/**
 * A callback interface for plugins to implement.  The methods are hierarchical,
 * so plugin authors can implement at the level that suits what their plugin needs to hear about.
 * @author mikeb
 */
public abstract class InputReader implements InputHandler {

    public final void handleDelivery(Delivery delivery, JSONObject config) throws PluginException {
        handleBodyAndConfig(delivery.getBody(), config);
    }

    public void handleBodyAndConfig(byte[] body, JSONObject config) throws PluginException {
        handleBody(body);
    }

    public void handleBody(byte[] body) throws PluginException {
        // do exactly nothing.  This is so that classes can override handleBodyAndConfig without supplying a handleBody.
    }

}
