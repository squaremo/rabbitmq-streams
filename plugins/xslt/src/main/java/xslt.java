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

import com.rabbitmq.streams.harness.InputReader;
import com.rabbitmq.streams.harness.PipelineComponent;
import net.sf.json.JSONObject;


public class xslt extends PipelineComponent {

    public InputReader input;

    public PipelinePublisher output;

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

    public xslt(final JSONObject config) throws IOException  {
        super(config);
        String xsltSrc = staticConfiguration.getString("stylesheet_url");
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

            @Override
            public void handleBodyAndConfig(byte[] body, JSONObject object) throws Exception,
                    InterruptedException {
                StreamSource xmlSource = new StreamSource(
                        new ByteArrayInputStream(body));
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
