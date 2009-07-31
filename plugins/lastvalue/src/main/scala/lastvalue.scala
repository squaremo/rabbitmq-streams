import java.io.IOException;
import java.util.HashMap;
import net.sf.json.JSONObject;

import com.rabbitmq.streams.harness.{InputReader, InputMessage};
import com.rabbitmq.streams.harness.AMQPInputConsumer.{PLUGIN_VALUES_HEADER => ValuesHeader}
import com.rabbitmq.streams.harness.PipelineComponent;
import com.rabbitmq.streams.harness.PluginException;
import com.fourspaces.couchdb.Document;

import scala.collection.mutable.Map
import scala.collection.jcl.Conversions._
import java.util.HashMap

class lastvalue() extends PipelineComponent() {

  override def configure(config : JSONObject) {
    // FIXME: AMQP has no byte type, so we can't put this in headers; likewise, CouchDB
    // wouldn't care for it either (except as an attachment perhaps).  So we make
    // it a string.
    var cachedValue: String = null;
    
    val value = new InputReader() {
      override def handleMessage(msg: InputMessage)
      {
        cachedValue = new String(msg.body());
        log.debug("Got cache value " + cachedValue)
        var state : java.util.Map[String, Object] = getState();
        
        if (state == null) state = new HashMap()
        
        if (state.contains(ValuesHeader))
        {
        	log.debug("CachedValue already in db")
            val currentCache = state.get(ValuesHeader);
            
            if (currentCache != cachedValue)
            {
            	log.debug("Setting cachedvalue since it has changed from " + currentCache + " to " + cachedValue)
              
            	state += (ValuesHeader -> cachedValue)
            	setState(state)
            }
        }
        else
        {
        	log.debug("Current value not there, setting")
          
        	state += (ValuesHeader -> cachedValue)
        	setState(state)
        }
        
      }
    }
    
    val input = new InputReader() {
      override def handleMessage(msg: InputMessage)
      {
            val headers = new HashMap[String, Object]();
            
            if (cachedValue != null)
            {
                //Check message against cachedValue
                headers += ("value" -> cachedValue);
            }
            
            //Send on message possibly modified
            try {
              publishToChannel("output", msg.withHeader(ValuesHeader, headers));
            } catch {
              case e: IOException =>
                throw new PluginException(e);
            }
      }
    }
    
    registerInput("input", input)
    registerInput("value", value)
  }

}
