import java.io.IOException;
import java.util.HashSet;
import java.util.Set;

import net.lshift.feedshub.harness.InputReader;
import net.lshift.feedshub.harness.Server;
import net.sf.json.JSONArray;
import net.sf.json.JSONObject;

import com.fourspaces.couchdb.Document;
import com.rabbitmq.client.QueueingConsumer.Delivery;

public class relay extends Server {

    private final Set<String> activeTerminals = new HashSet<String>();

    public final InputReader command = new InputReader() {

        public void handleDelivery(Delivery message) throws Exception {

            String serverIdterminalId = message.getEnvelope().getRoutingKey();
            int loc = serverIdterminalId.lastIndexOf('.');
            String serverIds = serverIdterminalId.substring(0, loc);
            String terminalId = serverIdterminalId.substring(loc + 1);

            Document terminalConfig = relay.this.terminalConfig(terminalId);
            Document terminalStatus = relay.this.terminalStatus(terminalId);

            String serverIdFromConfig = relay.this.config
                    .getString("server_id");
            if (!serverIds.contains(serverIdFromConfig)) {
                relay.this.log.fatal("Received a terminal status change "
                        + "message which was not routed for us: " + serverIds);
                return;
            }

            JSONArray terminalServers = terminalConfig.getJSONArray("servers");
            boolean found = false;
            for (int idx = 0; !found && idx < terminalServers.size(); ++idx) {
                found = terminalServers.getJSONObject(idx).getString("server")
                        .equals(serverIdFromConfig);
            }

            if (!found) {
                relay.this.log.fatal("Received a terminal status change "
                        + "message for a terminal which isn't "
                        + "configured for us: " + terminalServers);
                return;
            }

            relay.this.log.info("Received terminal status change for "
                    + terminalId);

            if (terminalStatus.getBoolean("active")) {
                activeTerminals.add(terminalId);
            } else {
                activeTerminals.remove(terminalId);
            }
            relay.this.ack(message);
        }
    };

    public final InputReader input = new InputReader() {

        public void handleDelivery(Delivery message) throws Exception {
            String terminalId = message.getEnvelope().getRoutingKey();
            if (activeTerminals.contains(terminalId)) {
                relay.this.output.publishWithKey(message.getBody(), terminalId);
            }
            relay.this.ack(message);
        }
    };

    public relay(JSONObject config) throws IOException {
        super(config);
        postConstructorInit();
    }

}
