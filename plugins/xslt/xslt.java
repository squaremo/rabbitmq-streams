import java.io.IOException;

import net.lshift.feedshub.harness.Plugin;
import net.lshift.feedshub.harness.Publisher;
import net.sf.json.JSONObject;

import com.rabbitmq.client.Consumer;

public class xslt extends Plugin {

	public Consumer input;

	public Publisher output;

	public xslt(final JSONObject config) throws IOException {
		super(config);
		// this can't be called from the superconstructor because input and
		// output won't be initialised until after the superconstructor
		init();
	}

	@Override
	public void run() throws IOException {
		if (null == output) {
			throw new IOException("INIT FAILED");
		}
		try {
			Thread.sleep(5000);
		} catch (InterruptedException e) {
		} // 5 seconds

		String msg = new JSONObject().accumulate("xslt alive", "hmm")
				.accumulate("_id", "0").toString();

		try {
			output.publish(msg.getBytes());
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
}
