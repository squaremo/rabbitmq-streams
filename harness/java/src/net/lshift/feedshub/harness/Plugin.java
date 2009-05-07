package net.lshift.feedshub.harness;

import java.io.IOException;
import java.lang.reflect.Field;
import java.net.URL;
import java.util.Iterator;

import net.sf.json.JSONArray;
import net.sf.json.JSONNull;
import net.sf.json.JSONObject;

import com.fourspaces.couchdb.Database;
import com.fourspaces.couchdb.Document;
import com.fourspaces.couchdb.Session;
import com.rabbitmq.client.Connection;
import com.rabbitmq.client.QueueingConsumer;
import com.rabbitmq.client.ShutdownSignalException;
import com.rabbitmq.client.AMQP.BasicProperties;
import com.rabbitmq.client.impl.ChannelN;

public abstract class Plugin implements Runnable {

    public static final String newline = System.getProperty("line.separator");
    static final BasicProperties basicPropsPersistent = new BasicProperties();
    {
        basicPropsPersistent.deliveryMode = 2;
    }

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
    final protected Logger log;

    public Plugin(final JSONObject config) throws IOException {
        this.config = config;
        pluginType = config.getJSONObject("plugin_type");
        JSONArray globalConfig = pluginType
                .getJSONArray("global_configuration_specification");
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

        String lrk = null;
        if (config.containsKey("server_id")) {
            String serverId = config.getString("server_id");
            String pluginName = config.getString("plugin_name");
            lrk = "." + serverId + "." + pluginName;
        } else {
            String feedId = config.getString("feed_id");
            String nodeId = config.getString("node_id");
            String pluginName = config.getString("plugin_name");
            lrk = "." + feedId + "." + pluginName + "." + nodeId;
        }
        logRoutingKey = lrk;

        log = new Logger(logChannel, logRoutingKey, basicPropsPersistent);
        new Thread(log).start();
        log.info("Starting up...");

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

    protected abstract Publisher publisher(String name, String exchange);

    protected abstract Runnable inputReaderRunnable(Field queueField,
            QueueingConsumer consumer);

    protected void dieHorribly() {
        System.exit(1);
    }

    private void noSuchField(String name) {
        log.fatal("No such field: " + name);
        dieHorribly();
    }

    private void illegalAccess(String name) {
        log.fatal("Illegal access: " + name);
        dieHorribly();
    }

    @SuppressWarnings("unchecked")
    protected void constructOutputs(JSONObject outputs) {
        for (Iterator<String> outKeysIt = (Iterator<String>) outputs.keys(); outKeysIt
                .hasNext();) {
            String name = (String) outKeysIt.next();
            String exchange = outputs.getString(name);
            try {
                Field outputField = Plugin.this.getClass().getField(name);
                outputField.set(Plugin.this, publisher(name, exchange));
            } catch (IllegalAccessException iac) {
                illegalAccess(name);
            } catch (NoSuchFieldException nsfe) {
                noSuchField(name);
            }
        }
    }

    @SuppressWarnings("unchecked")
    protected void constructInputs(JSONObject inputs) {
        for (Iterator<String> inKeysIt = (Iterator<String>) inputs.keys(); inKeysIt
                .hasNext();) {
            final String fieldName = inKeysIt.next();
            try {
                final Field pluginQueueField = getClass().getField(fieldName);
                final QueueingConsumer consumer = new QueueingConsumer(
                        messageServerChannel);
                messageServerChannel.basicConsume(inputs.getString(fieldName),
                        false, consumer);
                new Thread(inputReaderRunnable(pluginQueueField, consumer))
                        .start();
            } catch (NoSuchFieldException nsfe) {
                noSuchField(fieldName);
                // we could try-catch, but all we can do is let this bubble up
                // anyway
            } catch (IOException ioe) {
                log.fatal("IOException connecting to input " + fieldName);
                dieHorribly();
            }
        }
    }

    protected void postConstructorInit() throws IOException,
            IllegalArgumentException, SecurityException {

        // set up outputs FIRST, so we don't start processing messages
        // before we can put them anywhere
        JSONObject outputs = config.getJSONObject("outputs");
        constructOutputs(outputs);

        JSONObject inputs = config.getJSONObject("inputs");
        constructInputs(inputs);
    }

    public void run() {
    }

    public void shutdown() throws IOException {
        if (messageServerChannel.isOpen())
            try {
                messageServerChannel.close();
            } catch (ShutdownSignalException sse) {
            }
        log.shutdown();
        if (messageServerConnection.isOpen())
            try {
                messageServerConnection.close();
            } catch (ShutdownSignalException sse) {
            }
    }

    public final void start() throws Exception {
        new Thread(this).start();
    }

}
