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

	public xslt(final JSONObject config) throws IOException {
		super(config);
		String xsltSrc = configuration.getString("stylesheet_url");
		URLConnection xsltConn = new URL( xsltSrc ).openConnection();
		xsltConn.connect();
		final InputStream xsltFileContent = (InputStream) xsltConn.getContent();
		
		input = new Consumer() {

			public void handleCancelOk(String consumerTag) {

			}

			public void handleConsumeOk(String consumerTag) {
			}

			public void handleDelivery(String arg0, Envelope arg1,
					BasicProperties arg2, byte[] msg) throws IOException {
				StreamSource xmlSource = new StreamSource(
						new ByteArrayInputStream(msg));
				StreamSource xsltSource = new StreamSource(xsltFileContent);
				ByteArrayOutputStream output = new ByteArrayOutputStream();
				StreamResult result = new StreamResult(output);

				TransformerFactory transFact =
		                TransformerFactory.newInstance();
				try {
			        Transformer trans = transFact.newTransformer(xsltSource);
			        trans.transform(xmlSource, result);
	                xslt.this.output.publish(output.toByteArray());
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

	@Override
	public void run() throws IOException {
		// we don't actually do any work in this thread at all
		synchronized (lock) {
			while (!exit) {
				try {
					lock.wait();
				} catch (InterruptedException e) {
				}
			}
		}
	}
}
