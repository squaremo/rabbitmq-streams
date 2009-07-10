import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.rabbitmq.streams.harness.InputReader;
import com.rabbitmq.streams.harness.PipelineComponent;
import com.rabbitmq.streams.harness.PluginException;
import net.sf.json.JSONObject;

public class regexp_split extends PipelineComponent {

  public InputReader input;

  private final static String POSITIVE = "positive";
  private final static String NEGATIVE = "negative";

  public regexp_split(JSONObject configuration) throws IOException  {
    super(configuration);
    
    input = new InputReader() {
        @Override
        public void handleBodyAndConfig(byte[] body, JSONObject config) throws PluginException {
          String regexp = config.getString("regexp");
          int flags = (config.getBoolean("multiline") ? Pattern.MULTILINE : 0)
            | (config.getBoolean("caseinsensitive") ? Pattern.CASE_INSENSITIVE : 0)
            | (config.getBoolean("dotall") ? Pattern.DOTALL : 0);
          
          try {
            final Pattern pattern = Pattern.compile(regexp, flags);
            BufferedReader br = new BufferedReader(
                    new InputStreamReader(new ByteArrayInputStream(body)));
            StringBuilder sb = new StringBuilder();
            String line = br.readLine();
            while (null != line) {
              sb.append(line);
              sb.append(newline);
              line = br.readLine();
            }
            
            // Unless there's no body at all, we've added an extra line ending
            Matcher matcher = pattern.matcher(sb.substring(0, sb.length() > 0 ? sb.length() - newline.length() : 0));
            if (matcher.matches()) {
              publishToChannel(POSITIVE, body);
            } else {
              publishToChannel(NEGATIVE, body);
            }
          }
          catch (IOException ex) {
            throw new PluginException(ex);
          }
        }
      };
    postConstructorInit();
  }
}
