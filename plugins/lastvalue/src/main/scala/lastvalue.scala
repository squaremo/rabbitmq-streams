import java.io.IOException;
import java.util.HashMap;
import net.sf.json.JSONObject;

import com.rabbitmq.streams.harness.InputReader;
import com.rabbitmq.streams.harness.InputReaderRunnable.{PLUGIN_VALUES_HEADER => ValuesHeader};
import com.rabbitmq.streams.harness.PipelineComponent;
import com.rabbitmq.streams.harness.PluginException;
import com.fourspaces.couchdb.Document;

import scala.collection.mutable.Map
import scala.collection.jcl.Conversions._

object lastvalue
{
  val CachedValue = "cachedValue"
}

class lastvalue(config: JSONObject) extends PipelineComponent(config) {

  // FIXME: AMQP has no byte type, so we can't put this in headers; likewise, CouchDB
  // wouldn't care for it either (except as an attachment perhaps).  So we make
  // it a string.
    private var cachedValue: String = null;
    
    val cache = new InputReader() {
      override def handleBody(body: Array[Byte])
      {
        cachedValue = new String(body);
        log.debug("Got cache value " + cachedValue)
        var state = getState();
        
        if (state == null)
          state = new Document();
        
        if (state.has(ValuesHeader))
        {
        	log.debug("CachedValue already in db")
            val currentCache = state.get(ValuesHeader);
            
            if (currentCache != cachedValue)
            {
            	log.debug("Setting cachedvalue since it has changed from " + currentCache + " to " + cachedValue)
              
            	state.put(ValuesHeader, cachedValue)
            	setState(state)
            }
        }
        else
        {
        	log.debug("Current value not there, setting")
          
        	state.put(ValuesHeader, cachedValue)
        	setState(state)
        }
        
      }
    }
    
    val input = new InputReader() {
      override def handleBody(body: Array[Byte])
      {
            val headers = new HashMap[String, Object]();
            
            if (cachedValue != null)
            {
                //Check message against cachedValue
                headers += (ValuesHeader -> cachedValue);
            }
            
            //Send on message possibly modified
            try {
                getPublisher("output").publish(body, headers);
            } catch {
              case e: IOException =>
                throw new PluginException(e);
            }
      }
    }    
}
