import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStreamReader;

import com.rabbitmq.streams.harness.InputReader;
import com.rabbitmq.streams.harness.PipelineComponent;
import net.sf.json.JSONObject;

import com.rabbitmq.client.QueueingConsumer.Delivery;

public class logger extends PipelineComponent {

    public final InputReader input = new InputReader() {

        public void handleDelivery(Delivery message) throws Exception {
            byte[] body = message.getBody();

            BufferedReader br = new BufferedReader(new InputStreamReader(
                    new ByteArrayInputStream(body)));
            StringBuilder sb = new StringBuilder();
            String line = br.readLine();
            while (null != line) {
                sb.append(line);
                sb.append(newline);
                line = br.readLine();
            }

            logger.this.log.debug(sb.toString());
        }
    };

    public logger(JSONObject config) throws IOException {
        super(config);

        postConstructorInit();
    }
}
