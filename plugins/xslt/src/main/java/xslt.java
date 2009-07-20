import com.rabbitmq.streams.harness.InputReader;
import com.rabbitmq.streams.harness.PipelineComponent;
import com.rabbitmq.streams.harness.PluginException;
import net.sf.json.JSONObject;

import javax.xml.transform.*;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
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

  public xslt(final JSONObject config) throws IOException {
    super(config);
    String xsltSrc = config.getJSONObject("configuration").getString("stylesheet_url");
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
      public void handleBodyAndConfig(byte[] body, JSONObject object) throws PluginException {
        StreamSource xmlSource =
          new StreamSource(new ByteArrayInputStream(body));
        ByteArrayOutputStream output = new ByteArrayOutputStream();
        StreamResult result = new StreamResult(output);

        try {
          trans.transform(xmlSource, result);
        }
        catch (TransformerException e) {
          throw new PluginException(e);
        }
        String outputString = output.toString();
        xslt.this.publishToChannel("output", outputString.getBytes());
      }

    };

    registerHandler("input", input);
  }
}
