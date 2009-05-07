import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import net.lshift.feedshub.harness.InputReader;
import net.lshift.feedshub.harness.Server;
import net.sf.json.JSONObject;

import com.fourspaces.couchdb.Document;
import com.rabbitmq.client.QueueingConsumer.Delivery;

public class socket_source extends Server {

    public InputReader command;

    private final Map<String, SocketSource> terminalMap = new HashMap<String, SocketSource>();

    private final static class SocketSource implements Runnable {
        public SocketSource(Document terminalConfig) {
        }

        public void run() {
        }

        public void stop() {
        }
    }

    public socket_source(JSONObject config) throws IOException {
        super(config);

        command = new InputReader() {

            public void handleDelivery(Delivery message) throws Exception {

                String terminalId = message.getEnvelope().getRoutingKey();

                Document terminalConfig = socket_source.this.terminalsDatabase
                        .getDocument(terminalId);

                Document terminalStatus = socket_source.this.terminalsDatabase
                        .getDocument(terminalId + "_status");

                String serverIdFromTerminalConfig = terminalConfig
                        .getString("server");
                if (!serverIdFromTerminalConfig
                        .equals(socket_source.this.config
                                .getString("server_id"))) {
                    socket_source.this.log
                            .fatal("Received a terminal status change "
                                    + "message for a terminal which isn't "
                                    + "configured for us: "
                                    + serverIdFromTerminalConfig);
                }

                SocketSource source = terminalMap.get(terminalId);
                if (terminalStatus.getBoolean("active")) {
                    if (null == source) {
                        source = new SocketSource(terminalConfig);
                        terminalMap.put(terminalId, source);
                        new Thread(source).start();
                    }
                } else {
                    if (null != source) {
                        source.stop();
                    }
                }

                socket_source.this.ack(message);
            }
        };

        postConstructorInit();
    }

}
