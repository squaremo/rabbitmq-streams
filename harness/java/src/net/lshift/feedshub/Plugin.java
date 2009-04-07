package net.lshift.feedshub;

import java.io.IOException;

import com.rabbitmq.client.Channel;
import com.rabbitmq.client.Connection;

import net.sf.json.JSONObject;

public abstract class Plugin {

	final Connection messageServerConnection;
	final Channel messageServerChannel;
	
	Plugin (final JSONObject config) throws IOException {
		JSONObject messageServerSpec = config.getJSONObject("messageserver");
		messageServerConnection = AMQPConnection.amqConnectionFromConfig(messageServerSpec);
		messageServerChannel = messageServerConnection.createChannel();
	}
	
}
