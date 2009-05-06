import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.net.URLConnection;

import javax.xml.transform.ErrorListener;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
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

    private final ErrorListener xsltErrorLogger = new ErrorListener() {

        public void error(TransformerException exception)
                throws TransformerException {
            xslt.this.log.error(exception);
        }

        public void fatalError(TransformerException exception)
                throws TransformerException {
            xslt.this.log.fatal(exception);
        }

        public void warning(TransformerException exception)
                throws TransformerException {
            xslt.this.log.warn(exception);
        }
    };

    public xslt(final JSONObject config) throws IOException,
            IllegalArgumentException, SecurityException,
            IllegalAccessException, NoSuchFieldException {
        super(config);
        String xsltSrc = configuration.getString("stylesheet_url");
        URLConnection xsltConn = new URL(xsltSrc).openConnection();
        xsltConn.connect();
        InputStream xsltFileContent = (InputStream) xsltConn.getContent();
        StreamSource xsltSource = new StreamSource(xsltFileContent);

        TransformerFactory transFact = TransformerFactory.newInstance();
        transFact.setErrorListener(xsltErrorLogger);
        Transformer transTmp;
        try {
            transTmp = transFact.newTransformer(xsltSource);
        } catch (TransformerConfigurationException e) {
            log.fatal(e);
            transTmp = null;
            System.exit(1);
        }
        final Transformer trans = transTmp;
        trans.setErrorListener(xsltErrorLogger);

        input = new InputReader() {

            public void handleDelivery(Delivery message) throws Exception,
                    InterruptedException {
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
