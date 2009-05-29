import java.io.IOException;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.Socket;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import net.lshift.feedshub.harness.InputReader;
import net.lshift.feedshub.harness.Server;
import net.sf.json.JSONArray;
import net.sf.json.JSONObject;

import com.fourspaces.couchdb.Document;
import com.rabbitmq.client.QueueingConsumer.Delivery;

public class socket_destination extends Server {

    private final Map<String, List<SocketDestination>> terminalMap = new HashMap<String, List<SocketDestination>>();

    public final InputReader input = new InputReader() {

        public void handleDelivery(Delivery message) throws Exception {
            String terminalId = message.getEnvelope().getRoutingKey();
            List<SocketDestination> dests = terminalMap.get(terminalId);
            if (null != dests && 0 != dests.size()) {
                for (SocketDestination dest : dests) {
                    dest.send(message.getBody());
                }
            }
            socket_destination.this.ack(message);
        }
    };

    public final InputReader command = new InputReader() {

        public void handleDelivery(Delivery message) throws Exception {

            String serverIdterminalId = message.getEnvelope().getRoutingKey();
            int loc = serverIdterminalId.lastIndexOf('.');
            String serverIds = serverIdterminalId.substring(0, loc);
            String terminalId = serverIdterminalId.substring(loc + 1);

            Document terminalConfig = socket_destination.this
                    .terminalConfig(terminalId);
            Document terminalStatus = socket_destination.this
                    .terminalStatus(terminalId);

            String serverIdFromConfig = socket_destination.this.config
                    .getString("server_id");
            if (!serverIds.contains(serverIdFromConfig)) {
                socket_destination.this.log
                        .fatal("Received a terminal status change "
                                + "message which was not routed for us: "
                                + serverIds);
                return;
            }

            JSONArray terminalServers = terminalConfig.getJSONArray("servers");
            boolean found = false;
            List<JSONObject> termConfigObjects = new ArrayList<JSONObject>();
            for (int idx = 0; idx < terminalServers.size(); ++idx) {
                JSONObject obj = terminalServers.getJSONObject(idx);
                boolean match = obj.getString("server").equals(
                        serverIdFromConfig);
                found = found || match;
                if (match) {
                    termConfigObjects.add(obj.getJSONObject("destination"));
                }
            }

            if (!found) {
                socket_destination.this.log
                        .fatal("Received a terminal status change "
                                + "message for a terminal which isn't "
                                + "configured for us: " + terminalServers);
                return;
            }

            socket_destination.this.log
                    .info("Received terminal status change for " + terminalId);

            if (terminalStatus.getBoolean("active")) {
                List<SocketDestination> sources = terminalMap.get(terminalId);
                if (null == sources || 0 == sources.size()) {
                    try {
                        sources = new ArrayList<SocketDestination>(
                                termConfigObjects.size());
                        for (JSONObject termConfigObject : termConfigObjects) {
                            SocketDestination source = new SocketDestination(
                                    termConfigObject);
                            sources.add(source);
                            new Thread(source).start();
                        }
                        terminalMap.put(terminalId, sources);
                    } catch (IOException e) {
                        socket_destination.this.log.error(e);
                    }
                }
            } else {
                List<SocketDestination> sources = terminalMap.remove(terminalId);
                if (null != sources && 0 != sources.size()) {
                    for (SocketDestination source : sources) {
                        source.stop();
                    }
                }
            }

            socket_destination.this.ack(message);
        }
    };

    public socket_destination(JSONObject config) throws IOException {
        super(config);

        postConstructorInit();
    }

    private final class SocketDestination implements Runnable {
        final private int port;
        final private InetAddress address;
        final private Socket socket;
        final private OutputStream socketOutputStream;
        final private BlockingQueue<byte[]> sendQueue = new LinkedBlockingQueue<byte[]>();

        final private Object lockObj = new Object();
        private boolean running = true;

        public SocketDestination(JSONObject terminalConfig) throws IOException {
            port = terminalConfig.getInt("port");
            address = InetAddress.getByName(terminalConfig.getString("host"));
            socket = new Socket(address, port);
            socketOutputStream = socket.getOutputStream();
        }

        private boolean isRunning() {
            synchronized (lockObj) {
                return running;
            }
        }

        public void send(byte[] msg) {
            try {
                sendQueue.put(msg);
            } catch (InterruptedException e) {
                send(msg);
            }
        }

        public void run() {
            while (isRunning()) {
                try {
                    socketOutputStream.write(sendQueue.take());
                } catch (InterruptedException e) {
                    continue;
                } catch (IOException e) {
                    socket_destination.this.log.error(e);
                }
            }
            try {
                socket.close();
            } catch (IOException e) {
                socket_destination.this.log.error(e);
            }
        }

        public void stop() {
            synchronized (lockObj) {
                running = false;
            }
        }
    }

}
