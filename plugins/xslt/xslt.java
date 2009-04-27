import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.net.URL;
import java.net.URLConnection;

import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

import net.lshift.feedshub.harness.InputReader;
import net.lshift.feedshub.harness.Plugin;
import net.lshift.feedshub.harness.Publisher;
import net.sf.json.JSONObject;

import com.rabbitmq.client.QueueingConsumer.Delivery;

public class xslt extends Plugin {

	public InputReader input;

	public Publisher output;

	public xslt(final JSONObject config) throws Exception {
		super(config);
		String xsltSrc = configuration.getString("stylesheet_url");
		URLConnection xsltConn = new URL(xsltSrc).openConnection();
		xsltConn.connect();
		InputStream xsltFileContent = (InputStream) xsltConn.getContent();
		StreamSource xsltSource = new StreamSource(xsltFileContent);

		TransformerFactory transFact = TransformerFactory.newInstance();
		final Transformer trans = transFact.newTransformer(xsltSource);

		input = new InputReader() {

			public void handleDelivery(Delivery message) throws Exception {
				StreamSource xmlSource = new StreamSource(
						new ByteArrayInputStream(message.getBody()));
				ByteArrayOutputStream output = new ByteArrayOutputStream();
				StreamResult result = new StreamResult(output);

				trans.transform(xmlSource, result);
				String outputString = output.toString();
				xslt.this.output.publish(outputString.getBytes());
			}

		};
		postConstructorInit();
	}
}
