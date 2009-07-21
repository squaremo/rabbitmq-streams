package com.rabbitmq.streams.harness;

import net.sf.json.JSONObject;

/**
 * A callback interface for plugins to implement.  The methods are hierarchical,
 * so plugin authors can implement at the level that suits what their plugin needs to hear about.
 * @author mikeb@lshift.net
 */
public abstract class InputReader implements InputHandler {

    public void handleMessage(InputMessage m, JSONObject config) throws PluginException {
        handleMessage(m);
    }

    public void handleMessage(InputMessage m) throws PluginException {
        // Do exactly nothing.  Override this if you don't care for dynamic configuration.
    }

}
