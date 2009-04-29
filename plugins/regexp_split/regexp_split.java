import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import net.lshift.feedshub.harness.InputReader;
import net.lshift.feedshub.harness.Plugin;
import net.lshift.feedshub.harness.Publisher;
import net.sf.json.JSONObject;

import com.rabbitmq.client.QueueingConsumer.Delivery;

public class regexp_split extends Plugin {

	public InputReader input;

	public Publisher positive;
	public Publisher negative;

	public regexp_split(JSONObject config) throws IOException,
			IllegalArgumentException, SecurityException,
			IllegalAccessException, NoSuchFieldException {
		super(config);

		String regexp = configuration.getString("regexp");
		int flags = (configuration.getBoolean("multiline") ? Pattern.MULTILINE
				: 0)
				| (configuration.getBoolean("caseinsensitive") ? Pattern.CASE_INSENSITIVE
						: 0)
				| (configuration.getBoolean("dotall") ? Pattern.DOTALL : 0);

		final Pattern pattern = Pattern.compile(regexp, flags);

		input = new InputReader() {

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

				Matcher matcher = pattern.matcher(sb);
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
