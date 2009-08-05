package com.rabbitmq.streams.harness;

import java.io.IOException;

import net.sf.json.JSONObject;

import com.rabbitmq.client.Connection;
import com.rabbitmq.client.ConnectionFactory;
import com.rabbitmq.client.ConnectionParameters;

public class AMQPConnectionFactory {

    public Connection connectionFromConfig(final JSONObject config)
            throws IOException {
        String hostname = config.getString("host");
        int port = config.getInt("port");
        String virt = config.getString("virtual_host");
        String userid = config.getString("username");
        String password = config.getString("password");

        ConnectionParameters connParams = new ConnectionParameters();
        // no heartbeat -- useful for debugging
        // connParams.setRequestedHeartbeat(0);
        connParams.setVirtualHost(virt);
        connParams.setUsername(userid);
        connParams.setPassword(password);

        ConnectionFactory connFactory = new ConnectionFactory(connParams);

        return connFactory.newConnection(hostname, port);
    }

}
