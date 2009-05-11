import java.io.IOException;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.Socket;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import net.lshift.feedshub.harness.InputReader;
import net.lshift.feedshub.harness.Server;
import net.sf.json.JSONObject;

import com.fourspaces.couchdb.Document;
import com.rabbitmq.client.QueueingConsumer.Delivery;

public class socket_destination extends Server {

    private final Map<String, SocketDestination> terminalMap = new HashMap<String, SocketDestination>();

    public final InputReader input = new InputReader() {

        public void handleDelivery(Delivery message) throws Exception {
            String terminalId = message.getEnvelope().getRoutingKey();
            SocketDestination dest = terminalMap.get(terminalId);
            if (null != dest) {
                dest.send(message.getBody());
            }
            socket_destination.this.ack(message);
        }
    };

    public final InputReader command = new InputReader() {

        public void handleDelivery(Delivery message) throws Exception {

            String serverIdterminalId = message.getEnvelope().getRoutingKey();
            int loc = serverIdterminalId.indexOf('.');
            String serverId = serverIdterminalId.substring(0, loc);
            String terminalId = serverIdterminalId.substring(loc + 1);

            Document terminalConfig = socket_destination.this
                    .terminalConfig(terminalId);
            Document terminalStatus = socket_destination.this
                    .terminalStatus(terminalId);

            String serverIdFromTerminalConfig = terminalConfig
                    .getString("server");

            if (!serverId.equals(socket_destination.this.config
                    .getString("server_id"))) {
                socket_destination.this.log
                        .fatal("Received a terminal status change "
                                + "message which was not routed for us: "
                                + serverIdFromTerminalConfig);
                return;
            }

            if (!serverIdFromTerminalConfig
                    .equals(socket_destination.this.config
                            .getString("server_id"))) {
                socket_destination.this.log
                        .fatal("Received a terminal status change "
                                + "message for a terminal which isn't "
                                + "configured for us: "
                                + serverIdFromTerminalConfig);
                return;
            }

            socket_destination.this.log
                    .info("Received terminal status change for " + terminalId);

            SocketDestination source = terminalMap.get(terminalId);
            if (terminalStatus.getBoolean("active")) {
                if (null == source) {
                    try {
                        source = new SocketDestination(terminalConfig);
                        terminalMap.put(terminalId, source);
                        new Thread(source).start();
                    } catch (IOException e) {
                        log.error(e);
                    }
                }
            } else {
                if (null != source) {
                    source.stop();
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

        public SocketDestination(Document terminalConfig) throws IOException {
            JSONObject destinationConfig = terminalConfig
                    .getJSONObject("destination");
            port = destinationConfig.getInt("port");
            address = InetAddress
                    .getByName(destinationConfig.getString("host"));
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
