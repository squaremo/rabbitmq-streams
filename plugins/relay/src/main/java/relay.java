import com.rabbitmq.streams.harness.PluginException;
import com.rabbitmq.streams.harness.Server;
import com.rabbitmq.streams.harness.InputMessage;
import com.rabbitmq.streams.harness.NotificationType;
import com.rabbitmq.streams.harness.PluginBuildException;
import net.sf.json.JSONObject;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

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

  public Server.ServerInputReader input = new Server.ServerInputReader() {

      @Override
      public void handleBodyForTerminal(byte[] body, String key, InputMessage ack) throws PluginException {
        try {
          if (activeTerminals.contains(key)) {
            relay.this.publishToDestination(body, key);
          }
          ack.ack();
        }
        catch (Exception e) {
          notifier.notify(NotificationType.FatalError, "Couldn't relay msg: " + e.getMessage());
          throw new PluginException(e);
        }
      }
  };
  
  @Override
  public void configure(JSONObject config) throws PluginBuildException {
    super.configure(config);
    registerInput(input);
  }
}
