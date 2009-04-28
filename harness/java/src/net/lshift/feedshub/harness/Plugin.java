package net.lshift.feedshub.harness;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.lang.reflect.Field;
import java.net.URL;

import net.sf.json.JSONArray;
import net.sf.json.JSONNull;
import net.sf.json.JSONObject;

import com.fourspaces.couchdb.Database;
import com.fourspaces.couchdb.Document;
import com.fourspaces.couchdb.Session;
import com.rabbitmq.client.Connection;
import com.rabbitmq.client.QueueingConsumer;
import com.rabbitmq.client.AMQP.BasicProperties;
import com.rabbitmq.client.QueueingConsumer.Delivery;
import com.rabbitmq.client.impl.ChannelN;

public abstract class Plugin implements Runnable {
	
	static final private String logExchange = "feedshug/log";

	final protected Connection messageServerConnection;
	final protected ChannelN messageServerChannel;
	final private ChannelN logChannel;
	final private String logRoutingKey;
	final protected JSONObject pluginType;
	final protected JSONObject config;
	final protected JSONObject configuration;
	final private Database stateDb;
	final private String stateDocName;
	final protected Database privateDb;
	final BasicProperties basicPropsPersistent = new BasicProperties();

	protected Plugin(final JSONObject config) throws IOException {
		basicPropsPersistent.deliveryMode = 2; // persistent
		this.config = config;
		pluginType = config.getJSONObject("plugin_type");
		JSONArray globalConfig = pluginType
				.getJSONArray("global_configuration");
		JSONObject mergedConfig = new JSONObject();
		for (Object configItem : globalConfig) {
			JSONObject item = (JSONObject) configItem;
			mergedConfig.put(item.getString("name"), JSONObject.fromObject(item
					.get("value")));
		}
		JSONObject userConfig = config.getJSONObject("configuration");
		mergedConfig.putAll(userConfig);
		this.configuration = mergedConfig;

		JSONObject messageServerSpec = config.getJSONObject("messageserver");
		messageServerConnection = AMQPConnection
				.amqConnectionFromConfig(messageServerSpec);
		messageServerChannel = (ChannelN) messageServerConnection
				.createChannel();
		logChannel = (ChannelN) messageServerConnection.createChannel();

		String feedId = config.getString("feed_id");
		String nodeId = config.getString("node_id");
		String pluginName = config.getString("plugin_name");

		logRoutingKey = "." + feedId + "." + pluginName + "." + nodeId;

		URL dbURL = new URL(config.getString("state"));
		String path = dbURL.getPath();
		int loc = path.lastIndexOf('/');
		String db = path.substring(0, loc);

		Session couchSession = new Session(dbURL.getHost(), dbURL.getPort(),
				"", "");
		stateDocName = path.substring(1 + loc);
		stateDb = couchSession.getDatabase(db);

		Database privDb = null;
		if (config.has("database")
				&& !JSONNull.getInstance().equals(
						JSONObject.fromObject(config.get("database")))) {
			String privDbStr = config.getString("database");
			privDb = couchSession.createDatabase(privDbStr);
		}
		privateDb = privDb;
	}

	protected Document getState() throws IOException {
		return stateDb.getDocument(stateDocName);
	}

	protected void setState(Document state) throws IOException {
		stateDb.saveDocument(state, stateDocName);
	}

	protected void postConstructorInit() throws Exception {
		messageServerChannel.txSelect();

		// set up outputs FIRST
		JSONArray outputsAry = config.getJSONArray("outputs");
		JSONArray outputTypesAry = pluginType
				.getJSONArray("outputs_specification");

		for (int idx = 0; idx < outputsAry.size()
				&& idx < outputTypesAry.size(); ++idx) {
			final String exchange = outputsAry.getString(idx);
			final Publisher publisher = new Publisher() {

				public void publish(byte[] body) throws IOException {
					messageServerChannel.basicPublish(exchange, "",
							basicPropsPersistent, body);
				}

			};
			Field outputField = Plugin.this.getClass().getField(
					outputTypesAry.getJSONObject(idx).getString("name"));
			outputField.set(Plugin.this, publisher);
		}

		JSONArray inputsAry = config.getJSONArray("inputs");
		JSONArray inputTypesAry = pluginType
				.getJSONArray("inputs_specification");

		for (int idx = 0; idx < inputsAry.size() && idx < inputTypesAry.size(); ++idx) {
			final String fieldName = inputTypesAry.getJSONObject(idx)
					.getString("name");
			final Field pluginQueueField = getClass().getField(fieldName);
			final QueueingConsumer consumer = new QueueingConsumer(
					messageServerChannel);
			messageServerChannel.basicConsume(inputsAry.getString(idx), false,
					consumer);
			new Thread(new Runnable() {

				public void run() {
					while (true) {
						try {
							Delivery delivery = consumer.nextDelivery();
							Object pluginConsumer = pluginQueueField
									.get(Plugin.this);
							if (null != pluginConsumer)
								((InputReader) pluginConsumer)
										.handleDelivery(delivery);
							messageServerChannel.basicAck(delivery
									.getEnvelope().getDeliveryTag(), false);
							messageServerChannel.txCommit();
						} catch (Exception e) {
							e.printStackTrace();
							try {
								messageServerChannel.txRollback();
							} catch (IOException e1) {
								e1.printStackTrace();
							}
						}
					}

				}
			}).start();
		}
	}

	public void info(String message) throws IOException {
		String rk = "info" + logRoutingKey;
		logChannel.basicPublish(logExchange, rk, false, false,
				basicPropsPersistent, message.getBytes());
	}

	public void warn(String message) throws IOException {
		String rk = "warn" + logRoutingKey;
		logChannel.basicPublish(logExchange, rk, false, false,
				basicPropsPersistent, message.getBytes());
	}

	public void fatal(String message) throws IOException {
		String rk = "fatal" + logRoutingKey;
		logChannel.basicPublish(logExchange, rk, false, false,
				basicPropsPersistent, message.getBytes());
	}

	public void run() {
	}

	public void shutdown() throws IOException {
		messageServerChannel.close();
		messageServerConnection.close();
	}

	public final void start() throws Exception {
		new Thread(this).start();
		BufferedReader reader = new BufferedReader(new InputStreamReader(
				System.in));
		while (null != reader.readLine()) {
		}
		shutdown();
	}

}
