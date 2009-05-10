import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.HashMap;
import java.util.Map;

import net.lshift.feedshub.harness.InputReader;
import net.lshift.feedshub.harness.Server;
import net.sf.json.JSONObject;

import com.fourspaces.couchdb.Document;
import com.rabbitmq.client.QueueingConsumer.Delivery;

public class socket_source extends Server {

    private final Map<String, SocketSource> terminalMap = new HashMap<String, SocketSource>();

    public final InputReader command = new InputReader() {

        public void handleDelivery(Delivery message) throws Exception {

            String serverIdterminalId = message.getEnvelope().getRoutingKey();
            int loc = serverIdterminalId.indexOf('.');
            String serverId = serverIdterminalId.substring(0, loc);
            String terminalId = serverIdterminalId.substring(loc + 1);

            Document terminalConfig = socket_source.this
                    .terminalConfig(terminalId);
            Document terminalStatus = socket_source.this
                    .terminalStatus(terminalId);

            String serverIdFromTerminalConfig = terminalConfig
                    .getString("server");

            if (!serverId.equals(socket_source.this.config
                    .getString("server_id"))) {
                socket_source.this.log
                        .fatal("Received a terminal status change "
                                + "message which was not routed for us: "
                                + serverIdFromTerminalConfig);
                return;
            }

            if (!serverIdFromTerminalConfig.equals(socket_source.this.config
                    .getString("server_id"))) {
                socket_source.this.log
                        .fatal("Received a terminal status change "
                                + "message for a terminal which isn't "
                                + "configured for us: "
                                + serverIdFromTerminalConfig);
                return;
            }

            socket_source.this.log.info("Received terminal status change for "
                    + terminalId);

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

    public socket_source(JSONObject config) throws IOException {
        super(config);

        postConstructorInit();
    }

    private final class SocketSource implements Runnable {
        final private int port;
        final private String termId;
        final private Object lockObj = new Object();
        private boolean running = true;

        public SocketSource(Document terminalConfig) {
            port = terminalConfig.getJSONObject("source").getInt("port");
            termId = terminalConfig.getId();
        }

        private boolean isRunning() {
            synchronized (lockObj) {
                return running;
            }
        }

        public void run() {
            try {
                ServerSocket server = new ServerSocket(port);
                server.setReuseAddress(true);
                while (isRunning()) {
                    Socket sock = server.accept();
                    BufferedReader r = new BufferedReader(
                            new InputStreamReader(sock.getInputStream()));
                    String line = r.readLine();
                    while (null != line && isRunning()) {
                        socket_source.this.output.publishWithKey(line
                                .getBytes(), termId);
                        line = r.readLine();
                    }
                    sock.close();
                }
                server.close();
            } catch (IOException e) {
                e.printStackTrace();
            }
        }

        public void stop() {
            synchronized (lockObj) {
                running = false;
            }
        }
    }

}
