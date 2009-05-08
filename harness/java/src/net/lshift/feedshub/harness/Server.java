package net.lshift.feedshub.harness;

import java.io.IOException;
import java.lang.reflect.Field;
import java.net.URL;

import net.sf.json.JSONObject;

import com.fourspaces.couchdb.Database;
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

    public Server(JSONObject config) throws IOException {
        super(config);
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

    public static final class ServerPublisher implements Publisher {
        private String exchange;
        private Channel channel;

        ServerPublisher(String exchangeName, Channel out) {
            channel = out;
            exchange = exchangeName;
        }

        void publishWithKey(byte[] body, String key) throws IOException {
            channel.basicPublish(exchange, key, basicPropsPersistent, body);
        }
    }

    protected final void ack(Delivery delivery) throws IOException {
        this.messageServerChannel.basicAck(delivery.getEnvelope()
                .getDeliveryTag(), false);
    }

    public ServerPublisher output; // this is magically set on initialisation

    protected final void publishToDestination(byte[] body, String destination)
            throws IOException {
        output.publishWithKey(body, destination);
    }

    protected final Runnable inputReaderRunnable(final Field pluginQueueField,
            final QueueingConsumer consumer) {
        return new Runnable() {
            public void run() {
                // Subclasses must do their own acking and transactions
                while (messageServerChannel.isOpen()) {
                    try {
                        Delivery delivery = consumer.nextDelivery();
                        try {
                            Object pluginConsumer = pluginQueueField
                                    .get(Server.this);
                            if (null != pluginConsumer) {
                                ((InputReader) pluginConsumer)
                                        .handleDelivery(delivery);
                            }
                        } catch (Exception e) {
                            log.error(e);
                        }
                    } catch (InterruptedException _) {
                        // just continue around and try fetching again
                    }
                }
            }
        };
    }

    protected final Publisher publisher(final String name, final String exchange) {
        return new ServerPublisher(exchange, messageServerChannel);
    }

}
