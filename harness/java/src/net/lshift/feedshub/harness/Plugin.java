package net.lshift.feedshub.harness;

import java.io.IOException;
import java.lang.reflect.Field;

import net.sf.json.JSONArray;
import net.sf.json.JSONObject;

import com.rabbitmq.client.Channel;
import com.rabbitmq.client.Connection;
import com.rabbitmq.client.Consumer;
import com.rabbitmq.client.Envelope;
import com.rabbitmq.client.ShutdownSignalException;
import com.rabbitmq.client.AMQP.BasicProperties;

public abstract class Plugin {

	final protected Connection messageServerConnection;
	final protected Channel messageServerChannel;
	final protected JSONObject config;

	protected Plugin(final JSONObject config) throws IOException {
		this.config = config;
		JSONObject messageServerSpec = config.getJSONObject("messageserver");
		messageServerConnection = AMQPConnection
				.amqConnectionFromConfig(messageServerSpec);
		messageServerChannel = messageServerConnection.createChannel();
	}
	
	protected void init() throws IOException {
		JSONObject pluginType = config.getJSONObject("plugin_type");

		JSONArray inputsAry = config.getJSONArray("inputs");
		JSONArray inputTypesAry = pluginType.getJSONArray("inputs");

		for (int idx = 0; idx < inputsAry.size() && idx < inputTypesAry.size(); ++idx) {
			final String fieldName = inputTypesAry.getJSONObject(idx)
					.getString("name");
			Consumer callback;
			try {
				callback = new Consumer() {

					private final Field pluginQueueField = Plugin.this
							.getClass().getField(fieldName);

					public void handleCancelOk(String consumerTag) {
						try {
							Object consumer = pluginQueueField.get(Plugin.this);
							if (null != consumer)
								((Consumer) consumer)
										.handleCancelOk(consumerTag);
						} catch (IllegalArgumentException e) {
							e.printStackTrace();
						} catch (IllegalAccessException e) {
							e.printStackTrace();
						}
					}

					public void handleConsumeOk(String consumerTag) {
						try {
							Object consumer = pluginQueueField.get(Plugin.this);
							if (null != consumer)
								((Consumer) consumer)
										.handleConsumeOk(consumerTag);
						} catch (IllegalArgumentException e) {
							e.printStackTrace();
						} catch (IllegalAccessException e) {
							e.printStackTrace();
						}
					}

					public void handleDelivery(String arg0, Envelope arg1,
							BasicProperties arg2, byte[] arg3)
							throws IOException {
						try {
							Object consumer = pluginQueueField.get(Plugin.this);
							if (null != consumer)
								((Consumer) consumer).handleDelivery(arg0,
										arg1, arg2, arg3);
						} catch (IllegalArgumentException e) {
							e.printStackTrace();
						} catch (IllegalAccessException e) {
							e.printStackTrace();
						}
					}

					public void handleShutdownSignal(String consumerTag,
							ShutdownSignalException sig) {
						try {
							Object consumer = pluginQueueField.get(Plugin.this);
							if (null != consumer)
								((Consumer) consumer).handleShutdownSignal(
										consumerTag, sig);
						} catch (IllegalArgumentException e) {
							e.printStackTrace();
						} catch (IllegalAccessException e) {
							e.printStackTrace();
						}
					}

				};
				messageServerChannel.basicConsume(inputsAry.getString(idx),
						callback);
			} catch (NoSuchFieldException e) {
				e.printStackTrace();
				shutdown();
				System.exit(1);
			}
		}

		JSONArray outputsAry = config.getJSONArray("outputs");
		JSONArray outputTypesAry = pluginType.getJSONArray("outputs");

		final BasicProperties blankBasicProps = new BasicProperties();
		for (int idx = 0; idx < outputsAry.size()
				&& idx < outputTypesAry.size(); ++idx) {
			final String exchange = outputsAry.getString(idx);
			final Publisher publisher = new Publisher() {

				public void publish(byte[] body) throws IOException {
					System.out.println("Attempting to publish to " + exchange);
					messageServerChannel.basicPublish(exchange, "",
							blankBasicProps, body);
				}
			};
			try {
				Field outputField = Plugin.this.getClass().getField(
						outputTypesAry.getJSONObject(idx).getString("name"));
				outputField.set(Plugin.this, publisher);
			} catch (IllegalArgumentException e) {
				e.printStackTrace();
				shutdown();
				System.exit(1);
			} catch (SecurityException e) {
				e.printStackTrace();
				shutdown();
				System.exit(1);
			} catch (IllegalAccessException e) {
				e.printStackTrace();
				shutdown();
				System.exit(1);
			} catch (NoSuchFieldException e) {
				e.printStackTrace();
				shutdown();
				System.exit(1);
			}
		}
	}

	public void shutdown() throws IOException {
		messageServerChannel.close();
		messageServerConnection.close();
	}

	public abstract void run() throws IOException;

}
