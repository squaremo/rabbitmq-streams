import java.io.IOException;
import java.util.HashMap;
import net.sf.json.JSONObject;

import com.rabbitmq.streams.harness.InputReader;
import com.rabbitmq.streams.harness.PipelineComponent;
import com.rabbitmq.streams.harness.PipelineComponent.PipelinePublisher;
import com.rabbitmq.streams.harness.PluginException;
import com.rabbitmq.client.QueueingConsumer.Delivery;
import com.fourspaces.couchdb.Document;

import scala.collection.mutable.Map
import scala.collection.jcl.Conversions._

object headeroutput
{
  val CachedValue = "cachedValue"
}

class headeroutput(config: JSONObject) extends PipelineComponent(config) {
	import headeroutput.CachedValue;
 
   val input = new InputReader() {
      override def handleDelivery(delivery: Delivery, config: JSONObject)
      {
    	  	log.debug("Got a delivery");
    	  	val headersMap = delivery.getProperties().headers;
    	  	log.debug("The headers map has been set");
    	  	if (headersMap contains CachedValue)
    	  	{
    	  		log.debug("Headers map contains a cached value");
	            val header = headersMap(CachedValue)   
	            //Send on message possibly modified
	            try {
	                outputField.publish(header.toString().getBytes());
	            } catch {
	              case e: IOException =>
	                throw new PluginException(e);
	            }
            }
      }
    }
    
    private var outputField: PipelinePublisher = null;
    
    def output(p: PipelinePublisher)
    {
      outputField = p;
    }
    
    postConstructorInit();
}
