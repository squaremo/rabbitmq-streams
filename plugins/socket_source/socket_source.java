import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import net.lshift.feedshub.harness.InputReader;
import net.lshift.feedshub.harness.Server;
import net.sf.json.JSONArray;
import net.sf.json.JSONObject;

import com.fourspaces.couchdb.Document;
import com.rabbitmq.client.QueueingConsumer.Delivery;

public class socket_source extends Server {

    private final Map<String, List<SocketSource>> terminalMap = new HashMap<String, List<SocketSource>>();

    public final InputReader command = new InputReader() {

        public void handleDelivery(Delivery message) throws Exception {

            String serverIdterminalId = message.getEnvelope().getRoutingKey();
            int loc = serverIdterminalId.lastIndexOf('.');
            String serverIds = serverIdterminalId.substring(0, loc);
            String terminalId = serverIdterminalId.substring(loc + 1);

            Document terminalConfig = socket_source.this
                    .terminalConfig(terminalId);
            Document terminalStatus = socket_source.this
                    .terminalStatus(terminalId);

            String serverIdFromConfig = socket_source.this.config
                    .getString("server_id");
            if (!serverIds.contains(serverIdFromConfig)) {
                socket_source.this.log
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
                    termConfigObjects.add(obj.getJSONObject("source"));
                }
            }

            if (!found) {
                socket_source.this.log
                        .fatal("Received a terminal status change "
                                + "message for a terminal which isn't "
                                + "configured for us: " + terminalServers);
                return;
            }

            socket_source.this.log.info("Received terminal status change for "
                    + terminalId);

            if (terminalStatus.getBoolean("active")) {
                List<SocketSource> sources = terminalMap.get(terminalId);
                if (null == sources || 0 == sources.size()) {
                    sources = new ArrayList<SocketSource>(termConfigObjects
                            .size());
                    for (JSONObject termConfigObject : termConfigObjects) {
                        SocketSource source = new SocketSource(
                                termConfigObject, terminalId);
                        sources.add(source);
                        new Thread(source).start();
                    }
                    terminalMap.put(terminalId, sources);
                }
            } else {
                List<SocketSource> sources = terminalMap.remove(terminalId);
                if (null != sources && 0 != sources.size()) {
                    for (SocketSource source : sources) {
                        source.stop();
                    }
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
        final private Thread worker;
        private boolean running = true;
        private Socket sock = null;
        private ServerSocket server = null;

        public SocketSource(JSONObject termConfigObject, String terminalId) {
            port = termConfigObject.getInt("port");
            termId = terminalId;
            worker = Thread.currentThread();
        }

        private boolean isRunning() {
            synchronized (lockObj) {
                return running;
            }
        }

        public void run() {
            try {
                server = new ServerSocket(port);
                server.setReuseAddress(true);
                StringBuilder sb = new StringBuilder();
                while (isRunning()) {
                    sock = server.accept();
                    BufferedReader r = new BufferedReader(
                            new InputStreamReader(sock.getInputStream()));
                    String line = r.readLine();
                    while (null != line && isRunning()) {
                        sb.setLength(0);
                        sb.append(line);
                        sb.append(newline);
                        socket_source.this.output.publishWithKey(sb.toString()
                                .getBytes(), termId);
                        line = r.readLine();
                    }
                    sock.close();
                }
            } catch (IOException e) {
                socket_source.this.log.warn(e);
            } finally {
                try {
                    if (null != sock) {
                        sock.close();
                    }
                    if (null != server) {
                        server.close();
                    }
                } catch (IOException e2) {
                    socket_source.this.log.warn(e2);
                }
            }
        }

        public void stop() {
            synchronized (lockObj) {
                worker.interrupt();
                running = false;
                try {
                    if (null != sock) {
                        sock.close();
                    }
                    if (null != server) {
                        server.close();
                    }
                } catch (IOException e) {
                    socket_source.this.log.warn(e);
                }
            }
        }
    }

}
