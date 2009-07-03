
import com.rabbitmq.streams.harness.Server;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import net.reversehttp.HttpRequest;
import net.reversehttp.HttpServer;
import net.reversehttp.NormalHttpServer;
import net.reversehttp.RequestHandler;
import net.sf.json.JSONObject;

public class httppost extends Server implements RequestHandler {
    private final Object terminalMonitor = new Object();
    private final Map<String, List<JSONObject>> terminals = new HashMap<String, List<JSONObject>>();
    private final Map<String, String> paths = new HashMap<String, String>();

    private final HttpServer httpd;
    private final Thread httpdThread;

    public httppost(JSONObject config) throws IOException {
        super(config);

        //FIXME: make use of host
        //String host = (String) configuration.get("http_server_host");
        Object portObj = configuration.get("http_server_port");
        if ((portObj == null || !(portObj instanceof Integer))) {
            throw new IllegalArgumentException("Invalid http_server_port configuration value");
        }
        int port =(int) (Integer) portObj;

        httpd = new NormalHttpServer(port, this);
        httpdThread = new Thread(new Runnable() {
            public void run() {
                try {
                    httpd.serve();
                } catch (IOException ioe) {
                    log.error(ioe);
                    System.exit(2); // FIXME: should be clean API for shutdown
                }
            }
        });
        httpdThread.setDaemon(true);
        httpdThread.start();
        
        postConstructorInit();
    }

    protected void terminalStatusChange(String terminalId,
                                        List<JSONObject> terminalConfigs,
                                        boolean active) {
        synchronized (terminalMonitor) {
            for (String p : terminalUrlPaths(terminalId)) {
                paths.remove(p);
            }
            terminals.remove(terminalId);

            if (active) {
                terminals.put(terminalId, terminalConfigs);
                for (String p : terminalUrlPaths(terminalId)) {
                    paths.put(p, terminalId);
                }
            }
        }
    }

    private List<String> terminalUrlPaths(String terminalId) {
        synchronized (terminalMonitor) {
            ArrayList<String> result = new ArrayList<String>();
            List<JSONObject> configs = terminals.get(terminalId);
            if (configs != null) {
                for (JSONObject c : configs) {
                    result.add((String) ((JSONObject) c.get("source")).get("url_path"));
                }
            }
            return result;
        }
    }

    private String terminalForPath(String p) {
        synchronized (terminalMonitor) {
            return paths.get(p);
        }
    }

    private static byte[] utf8Encode(String s) {
        try {
            return s.getBytes("UTF-8");
        } catch (UnsupportedEncodingException uee) {
            throw new RuntimeException("Fatal system error: UTF-8 encoding missing");
        }
    }

    public void handleRequest(HttpRequest req) {
        req.setResponse(200, "OK");
        req.getResponse().setHeader("Content-type", "text/plain; charset=utf-8");
        req.getResponse().setBody(utf8Encode("Hello world"));
    }
}