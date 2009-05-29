package net.lshift.feedshub.harness;

import java.io.IOException;
import java.net.URL;
import java.util.List;
import java.util.ArrayList;

import net.sf.json.JSONObject;
import net.sf.json.JSONArray;

import com.fourspaces.couchdb.Database;
import com.fourspaces.couchdb.Document;
import com.fourspaces.couchdb.Session;
import com.rabbitmq.client.Channel;
import com.rabbitmq.client.QueueingConsumer;
import com.rabbitmq.client.QueueingConsumer.Delivery;

/**
 * A superclass for terminal servers (ho ho). These have predefined inputs and
 * outputs, rather than having them specified, and don't enforce transactions.
 */
public abstract class Server extends Plugin {

    final private URL terminalsDbUrl;
    final protected Database terminalsDatabase;
    final protected String serverId;

    public ServerPublisher output; // this is magically set on initialisation

    public Server(JSONObject config) throws IOException {
        super(config);
        this.serverId = config.getString("server_id");
        String terminalsDbStr = config.getString("terminals_database");
        terminalsDbUrl = new URL(terminalsDbStr);

        Session couchSession = new Session(terminalsDbUrl.getHost(),
                terminalsDbUrl.getPort(), "", "");
        String path = terminalsDbUrl.getPath();
        int loc;
        if (path.endsWith("/")) {
            loc = path.substring(0, path.length() - 1).lastIndexOf('/');
        } else {
            loc = path.lastIndexOf('/');
        }
        String terminalsDbName = path.substring(loc);
        terminalsDatabase = couchSession.getDatabase(terminalsDbName);
    }

    protected final Runnable inputReaderRunnable(final Plugin.Getter getter,
            final QueueingConsumer consumer) {
        return new Runnable() {
            public void run() {
                // Subclasses must do their own acking and transactions
                while (Server.this.messageServerChannel.isOpen()) {
                    try {
                        Delivery delivery = consumer.nextDelivery();
                        try {
                            InputReader pluginConsumer = getter.get();
                            if (null != pluginConsumer) {
                                pluginConsumer.handleDelivery(delivery);
                            } else {
                                Server.this.log
                                        .warn("No non-null input reader field ");
                            }
                        } catch (Exception e) {
                            Server.this.log.error(e);
                        }
                    } catch (InterruptedException _) {
                        // just continue around and try fetching again
                    }
                }
            }
        };
    }

    protected final void ack(Delivery delivery) throws IOException {
        this.messageServerChannel.basicAck(delivery.getEnvelope()
                .getDeliveryTag(), false);
    }

    protected final Publisher publisher(final String name, final String exchange) {
        return new ServerPublisher(exchange, messageServerChannel);
    }

    protected final void publishToDestination(byte[] body, String destination)
            throws IOException {
        output.publishWithKey(body, destination);
    }

  /**
   * Get all the configurations particular to this server
   */
    protected final List<JSONObject> terminalConfigs(String terminalId)
            throws IOException {
        Document wholeConfig = this.terminalsDatabase.getDocument(terminalId);
        JSONArray servers = wholeConfig.getJSONArray("servers");
        ArrayList<JSONObject> configs = new ArrayList<JSONObject>();
        for (int i=0; i < servers.size(); i++) {
          JSONObject config = servers.getJSONObject(i);
          if (this.serverId.equals(config.getString("server"))) {
            configs.add(config);
          }
        }
        return configs;
    }

    protected final Document terminalStatus(String terminalId)
            throws IOException {
        return this.terminalsDatabase.getDocument(terminalId + "_status");
    }

    public static final class ServerPublisher implements Publisher {
        private String exchange;
        private Channel channel;

        ServerPublisher(String exchangeName, Channel out) {
            channel = out;
            exchange = exchangeName;
        }

        public void publishWithKey(byte[] body, String key) throws IOException {
            channel.basicPublish(exchange, key, basicPropsPersistent, body);
        }
    }

    public final InputReader command = new InputReader() {

        public void handleDelivery(Delivery message) throws Exception {

            String serverIdterminalId = message.getEnvelope().getRoutingKey();
            int loc = serverIdterminalId.lastIndexOf('.');
            String serverIds = serverIdterminalId.substring(0, loc);
            String terminalId = serverIdterminalId.substring(loc + 1);

            List<JSONObject> terminalConfigs = Server.this
                    .terminalConfigs(terminalId);
            Document terminalStatus = Server.this.terminalStatus(terminalId);

            if (!serverIds.contains(Server.this.serverId)) {
                Server.this.log
                        .error("Received a terminal status change "
                                + "message which was not routed for us: "
                                + serverIds);
                return;
            }

            if (terminalConfigs.size()==0) {
                Server.this.log
                        .error("Received a terminal status change "
                                + "message for a terminal which isn't "
                                + "configured for us: " + terminalConfigs);
                return;
            }

            Server.this.log
                    .info("Received terminal status change for " + terminalId);

            Server.this.terminalStatusChange(terminalId,
                                             terminalConfigs,
                                             terminalStatus.getBoolean("active"));
            Server.this.ack(message);
        }
    };

  /**
   * Handle a status change.  This will be called from another thread,
   * so take care.
   */
  protected abstract void terminalStatusChange(String terminalId,
                                               List<JSONObject> configs,
                                               boolean active);

}
