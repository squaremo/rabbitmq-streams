import java.io.IOException;
import java.util.HashMap;
import net.sf.json.JSONObject;

import com.rabbitmq.streams.harness.InputReader;
import com.rabbitmq.streams.harness.PipelineComponent;
import com.rabbitmq.streams.harness.PipelineComponent.PipelinePublisher;
import com.rabbitmq.streams.harness.PluginException;
import com.fourspaces.couchdb.Document;

import scala.collection.mutable.Map
import scala.collection.jcl.Conversions._

object onevaluecache
{
  val CachedValue = "cachedValue"
}

class onevaluecache(config: JSONObject) extends PipelineComponent(config) {
	import onevaluecache.CachedValue;
 
    private var cachedValue: String = null;
    
    val cache = new InputReader() {
      override def handleBody(body: Array[Byte])
      {
        cachedValue = new String(body);
        
        var state = getState();
        
        if (state == null)
          state = new Document();
        
        if (state.has(CachedValue))
        {
            val currentCache = state.get(CachedValue);
            
            if (currentCache != cachedValue)
            {
            	state.put(CachedValue, cachedValue)
            	setState(state)
            }
        }
        else
        {
        	state.put(CachedValue, cachedValue)
        	setState(state)
        }
        
      }
    }
    
    val input = new InputReader() {
      override def handleBody(body: Array[Byte])
      {
            val headers = new HashMap[java.lang.String, java.lang.Object]();
            
            if (cachedValue != null)
            {
                //Check message against cachedValue
                headers += (CachedValue -> cachedValue);
            }
            
            //Send on message possibly modified
            try {
                outputField.publish(body, headers);
            } catch {
              case e: IOException =>
                throw new PluginException(e);
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