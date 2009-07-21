import com.rabbitmq.streams.harness.InputReader;
import com.rabbitmq.streams.harness.InputMessage;
import com.rabbitmq.streams.harness.PipelineComponent;
import com.rabbitmq.streams.harness.PluginException;
import net.sf.json.JSONObject;

import javax.xml.transform.*;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.net.URL;
import java.net.URLConnection;

public class xslt extends PipelineComponent {

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

  public void configure(final JSONObject config) throws PluginException {
    try {
      String xsltSrc = config.getString("stylesheet_url");
      URLConnection xsltConn = new URL(xsltSrc).openConnection();
      xsltConn.connect();
      InputStream xsltFileContent = (InputStream) xsltConn.getContent();
      StreamSource xsltSource = new StreamSource(xsltFileContent);
      TransformerFactory transFact = TransformerFactory.newInstance();
      transFact.setErrorListener(xsltErrorLogger);
      Transformer transTmp;
      try {
        transTmp = transFact.newTransformer(xsltSource);
      }
      catch (TransformerConfigurationException e) {
        log.fatal(e);
        transTmp = null;
        System.exit(1);
      }
      final Transformer trans = transTmp;
      trans.setErrorListener(xsltErrorLogger);

      InputReader input = new InputReader() {

        @Override
        public void handleMessage(InputMessage msg, JSONObject config) throws PluginException {
          StreamSource xmlSource =
                  new StreamSource(new ByteArrayInputStream(msg.body()));
          ByteArrayOutputStream output = new ByteArrayOutputStream();
          StreamResult result = new StreamResult(output);

          try {
            trans.transform(xmlSource, result);
            String outputString = output.toString();
            xslt.this.publishToChannel("output", msg.withBody(outputString));
          }
          catch (Exception e) {
            throw new PluginException(e);
          }
        }

      };
      registerInput("input", input);
    }
    catch (Exception ex) {
      throw new PluginException(ex);
    }
  }
}
