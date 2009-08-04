package com.rabbitmq.streams.plugins.xpathselect

import com.rabbitmq.streams.harness.PipelineComponent
import net.sf.json.JSONObject
import javax.xml.xpath.XPathFactory
import javax.xml.xpath.XPathExpressionException
import com.rabbitmq.streams.harness.PluginBuildException
import com.rabbitmq.streams.harness.InputReader
import org.xml.sax.InputSource
import javax.xml.xpath.XPathConstants
import java.io.ByteArrayInputStream
import org.w3c.dom.NodeList
import org.w3c.dom.Node
import javax.xml.transform.TransformerFactory
import java.io.ByteArrayOutputStream
import javax.xml.transform.stream.StreamResult
import javax.xml.transform.dom.DOMSource
import com.rabbitmq.streams.harness.InputMessage
import org.w3c.dom.Text

/**
 * XPathSelect use the supplied expression to select nodes from the input;
 * it then publishes each result, serialised.
 *
 */
class XPathSelect extends PipelineComponent {
  override def configure(config : JSONObject) {
    val compilerfactory = XPathFactory.newInstance
    val identity = TransformerFactory.newInstance().newTransformer()
    val compiler = compilerfactory.newXPath
    object input extends InputReader {
      def serialise(node : Node) : Array[Byte] = node match {
          case t : Text => t.getWholeText().getBytes
          case _ => {
              val buffer = new ByteArrayOutputStream();
              val dest = new StreamResult(buffer);
              identity.transform(new DOMSource(node), dest)
              buffer.toByteArray
          }
      }

      override def handleMessage(msg : InputMessage, config : JSONObject) {
        identity.reset
        val expression = config.getString("expression")
        try {
          val res = (compiler.evaluate(expression,
                                       new InputSource(new ByteArrayInputStream(msg.body)),
                                       XPathConstants.NODESET)).asInstanceOf[NodeList]
          for {
            i <- 0 to (res.getLength - 1);
            node = res.item(i)
          } publishToChannel("output", msg.withBody(serialise(node)))
        }
        catch {
          case ex : XPathExpressionException => log.error(ex); throw new PluginBuildException("Cannot compile expression " + expression, ex)
          case ex : Exception => log.error(ex); throw new PluginBuildException("Could not evaluate expression", ex)
        }
      }
    }
    registerInput("input", input)
  }
}
