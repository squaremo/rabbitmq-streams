import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.rabbitmq.streams.harness.InputReader;
import com.rabbitmq.streams.harness.PipelineComponent;
import net.sf.json.JSONObject;

import com.rabbitmq.client.QueueingConsumer.Delivery;

public class regexp_split extends PipelineComponent {

    public InputReader input;

    public PipelinePublisher positive;
    public PipelinePublisher negative;

    public regexp_split(JSONObject configuration) throws IOException  {
        super(configuration);

        input = new InputReader() {

            @Override
            public void handleBodyAndConfig(byte[] body, JSONObject config) throws Exception {
                String regexp = config.getString("regexp");
                int flags = (config.getBoolean("multiline") ? Pattern.MULTILINE : 0)
                        | (config.getBoolean("caseinsensitive") ? Pattern.CASE_INSENSITIVE : 0)
                        | (config.getBoolean("dotall") ? Pattern.DOTALL : 0);

                final Pattern pattern = Pattern.compile(regexp, flags);

                BufferedReader br = new BufferedReader(new InputStreamReader(
                        new ByteArrayInputStream(body)));
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
                    positive.publish(body);
                } else {
                    negative.publish(body);
                }
            }
        };

        postConstructorInit();
    }
}
