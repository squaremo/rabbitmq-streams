package net.lshift.feedshub.harness;

import java.io.IOException;
import java.net.URL;

import net.sf.json.JSONObject;

import com.fourspaces.couchdb.Database;
import com.fourspaces.couchdb.Document;
import com.fourspaces.couchdb.Session;
import com.rabbitmq.client.Channel;
import com.rabbitmq.client.QueueingConsumer;
import com.rabbitmq.client.QueueingConsumer.Delivery;

/**
 * A superclass for pipeline components. THis does a bit more work than its
 * superclass, Plugin; in particular, in wrapping a transaction around
 * handleDelivery.
 */
public abstract class PipelineComponent extends Plugin {

    final private Object privateLock = new Object();
    final private Database stateDb;
    final private String stateDocName;

    public PipelineComponent(JSONObject config) throws IOException {
        super(config);
        URL dbURL = new URL(config.getString("state"));
        String path = dbURL.getPath();
        int loc = path.lastIndexOf('/'); // minus document
        String db = path.substring(0, loc);
        int loc2 = db.lastIndexOf('/');
        String dbName = db.substring(loc2);

        Session couchSession = new Session(dbURL.getHost(), dbURL.getPort(),
                "", "");
        stateDocName = path.substring(1 + loc);
        stateDb = couchSession.getDatabase(dbName);
    }

    protected final Publisher publisher(final String name, final String exchange) {
        return new PipelinePublisher(exchange, messageServerChannel);
    }

    protected final void constructOutputs(JSONObject outputs) {
        try {
            messageServerChannel.txSelect();
        } catch (IOException ioe) {
            log.fatal("Cannot switch channel to transactional mode");
            dieHorribly();
        }
        super.constructOutputs(outputs);
    }

    protected final Runnable inputReaderRunnable(final Plugin.Getter getter,
            final QueueingConsumer consumer) {
        return new Runnable() {
            public void run() {
                while (PipelineComponent.this.messageServerChannel.isOpen()) {
                    try {
                        // We may have many input queues, so to avoid
                        // interleaving transactions,
                        // we have to choose either to have a channel for each,
                        // or serialise the
                        // message handing.
                        // Since there are a maximum of 15 channels, we choose
                        // to serialise
                        // message handling by way of this mutex.
                        // Note: Transactions are only on outgoing messages, so
                        // it doesn't
                        // matter that two or more threads could receive
                        // messages before one
                        // acquires the lock; the transaction will be complete
                        // or abandoned
                        // before another consumer can start sending anything.
                        Delivery delivery = consumer.nextDelivery();
                        synchronized (privateLock) {
                            try {
                                InputReader pluginConsumer = getter.get();
                                if (null != pluginConsumer) {
                                    pluginConsumer.handleDelivery(delivery);
                                    PipelineComponent.this.messageServerChannel
                                            .basicAck(delivery.getEnvelope()
                                                    .getDeliveryTag(), false);
                                    PipelineComponent.this.messageServerChannel
                                            .txCommit();
                                } else {
                                    PipelineComponent.this.log
                                            .warn("No non-null input reader field ");
                                }
                            } catch (Exception e) {
                                try {
                                    PipelineComponent.this.log.error(e);
                                    PipelineComponent.this.messageServerChannel
                                            .txRollback();
                                } catch (IOException e1) {
                                    PipelineComponent.this.log.error(e1);
                                }
                            }
                        }
                    } catch (InterruptedException _) {
                    }
                }
            }

        };
    }

    protected Document getState() throws IOException {
        return stateDb.getDocument(stateDocName);
    }

    protected void setState(Document state) throws IOException {
        stateDb.saveDocument(state, stateDocName);
    }

    public static final class PipelinePublisher implements Publisher {
        private String exchange;
        private Channel channel;

        public PipelinePublisher(String exchangeName, Channel out) {
            exchange = exchangeName;
            channel = out;
        }

        public void publish(byte[] body) throws IOException {
            channel.basicPublish(exchange, "", basicPropsPersistent, body);
        }
    }

}
