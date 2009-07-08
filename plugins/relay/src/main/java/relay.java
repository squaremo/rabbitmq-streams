import java.io.IOException;
import java.util.HashSet;
import java.util.Set;
import java.util.List;

import com.rabbitmq.streams.harness.InputReader;
import com.rabbitmq.streams.harness.Server;
import com.rabbitmq.streams.harness.PluginException;
import net.sf.json.JSONObject;

// TODO: update to deal with multiple source/destinations
public class relay extends Server {

  private final Set<String> activeTerminals = new HashSet<String>();

  protected void terminalStatusChange(String terminalId,
                                      List<JSONObject> terminalConfigs,
                                      boolean active) {
    if (active) {
      activeTerminals.add(terminalId);
    }
    else {
      activeTerminals.remove(terminalId);
    }
  }

  public final InputReader input = new Server.ServerInputReader() {
      
      @Override
      public void handleBodyForTerminal(byte[] body, String key, long tag) throws PluginException {
        try {
          if (activeTerminals.contains(key)) {
            relay.this.output.publishWithKey(body, key);
          }
          relay.this.ack(tag);
        }
        catch (IOException ex) {
          throw new PluginException(ex);
        }
      }
  };
  
  public relay(JSONObject config) throws IOException {
    super(config);
    postConstructorInit();
  }
}
