import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.net.URLConnection;

import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

import net.lshift.feedshub.harness.Plugin;
import net.lshift.feedshub.harness.Publisher;
import net.sf.json.JSONObject;

import com.rabbitmq.client.Consumer;
import com.rabbitmq.client.Envelope;
import com.rabbitmq.client.ShutdownSignalException;
import com.rabbitmq.client.AMQP.BasicProperties;

public class xslt extends Plugin {

	public Consumer input;

	public Publisher output;

	private boolean exit = false;

	private final Object lock = new Object();

	public xslt(final int pid, final JSONObject config) throws Exception {
		super(pid, config);
		String xsltSrc = configuration.getString("stylesheet_url");
		URLConnection xsltConn = new URL(xsltSrc).openConnection();
		xsltConn.connect();
		InputStream xsltFileContent = (InputStream) xsltConn.getContent();
		StreamSource xsltSource = new StreamSource(xsltFileContent);

		TransformerFactory transFact = TransformerFactory.newInstance();
		final Transformer trans = transFact.newTransformer(xsltSource);

		input = new Consumer() {

			public void handleCancelOk(String consumerTag) {

			}

			public void handleConsumeOk(String consumerTag) {
			}

			public void handleDelivery(String consumerTag, Envelope envelope,
					BasicProperties arg2, byte[] msg) throws IOException {
				StreamSource xmlSource = new StreamSource(
						new ByteArrayInputStream(msg));
				ByteArrayOutputStream output = new ByteArrayOutputStream();
				StreamResult result = new StreamResult(output);

				try {
					trans.transform(xmlSource, result);
					String outputString = output.toString();
					xslt.this.output.publish(outputString.getBytes());
					xslt.this.output.acknowledge(envelope.getDeliveryTag());
				} catch (TransformerConfigurationException e) {
					e.printStackTrace();
					System.exit(1);
				} catch (TransformerException e) {
					e.printStackTrace();
					System.exit(1);
				}
			}

			public void handleShutdownSignal(String consumerTag,
					ShutdownSignalException sig) {
				synchronized (lock) {
					exit = true;
					lock.notifyAll();
				}
				try {
					shutdown();
				} catch (IOException e) {
					e.printStackTrace();
				}
			}

		};
		// init can't be called from the superconstructor because input and
		// output won't be initialised until after the superconstructor
		init();
	}
}
