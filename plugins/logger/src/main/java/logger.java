import com.rabbitmq.streams.harness.InputReader;
import com.rabbitmq.streams.harness.InputMessage;
import com.rabbitmq.streams.harness.PipelineComponent;
import com.rabbitmq.streams.harness.PluginBuildException;
import com.rabbitmq.streams.harness.PluginException;
import net.sf.json.JSONObject;

import java.nio.charset.Charset;


public class logger extends PipelineComponent {

  private InputReader input = new InputReader() {
    @Override
    public void handleMessage(InputMessage msg) throws PluginException {
      // FIXME(alexander): push this into msg, once the unicode stuff is merged in
      logger.this.log.debug(new String(msg.body(), Charset.forName("UTF-8")));
    }
  };
  @Override
  public void configure(JSONObject config) throws PluginBuildException {
    registerInput("input", input);
  }
}
