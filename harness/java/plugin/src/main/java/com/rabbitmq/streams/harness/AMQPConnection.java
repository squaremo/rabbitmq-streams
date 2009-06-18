package com.rabbitmq.streams.harness;

import java.io.IOException;

import net.sf.json.JSONObject;

import com.rabbitmq.client.Connection;
import com.rabbitmq.client.ConnectionFactory;
import com.rabbitmq.client.ConnectionParameters;

public class AMQPConnection {

    public static Connection amqConnectionFromConfig(final JSONObject config)
            throws IOException {
        String hostname = config.getString("host");
        int port = config.getInt("port");
        String virt = config.getString("virtual_host");
        String userid = config.getString("username");
        String password = config.getString("password");

        ConnectionParameters connParams = new ConnectionParameters();
        connParams.setVirtualHost(virt);
        connParams.setUsername(userid);
        connParams.setPassword(password);

        ConnectionFactory connFactory = new ConnectionFactory(connParams);

        return connFactory.newConnection(hostname, port);
    }

}
