import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStreamReader;

import com.rabbitmq.streams.harness.InputReader;
import com.rabbitmq.streams.harness.PipelineComponent;
import com.rabbitmq.streams.harness.PluginException;
import net.sf.json.JSONObject;

public class logger extends PipelineComponent {

  public final InputReader input = new InputReader() {
      
      @Override
      public void handleBody(byte[] body) throws PluginException {
        BufferedReader br =
          new BufferedReader(new InputStreamReader(new ByteArrayInputStream(body)));
        StringBuilder sb = new StringBuilder();
        try {
          String line = br.readLine();
          while (null != line) {
            sb.append(line);
            sb.append(newline);
            line = br.readLine();
          }
        }
        catch (IOException ex) {
          throw new PluginException(ex);
        }
        
        logger.this.log.debug(sb.toString());
      }
    };

  public logger(JSONObject config) throws IOException {
    super(config);
    postConstructorInit();
  }
}
