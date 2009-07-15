
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import net.sf.json.JSONObject;

import com.fourspaces.couchdb.Document;
import com.rabbitmq.streams.harness.InputReader;
import com.rabbitmq.streams.harness.PipelineComponent;
import com.rabbitmq.streams.harness.PluginException;


public class slowtestplugin extends PipelineComponent {

    protected static final String CachedValue = "cachedValue";
    private String cachedValue = null;
    
    public final InputReader cache = new InputReader() 
    {

        public void handleBody(byte[] body) throws PluginException {
            cachedValue = new String(body);
            log.debug("Cached value is " + cachedValue);
            String savedValue;
			try {
				savedValue = getSavedCache();
				log.debug("Saved value is " + savedValue);
			
            if (savedValue != cachedValue)
                saveValue(cachedValue);
            
			} catch (IOException e) {
				throw new PluginException(e);
			}
			
            //Send on message possibly modified
            try {
                output.publish("I got a message".getBytes());
            } catch (IOException e) {
                throw new PluginException(e);
            }
            
        }

    };
/*
    public final InputReader input = new InputReader() {

        public void handleBody(byte[] body) throws PluginException {
            Map headers = new HashMap();
            
            if (cachedValue != null)
            {
                //Check message against cachedValue
                headers.put(CachedValue, cachedValue);
            }
            
            //Send on message possibly modified
            try {
                output.publish(body, headers);
            } catch (IOException e) {
                throw new PluginException(e);
            }
        }

    };
    */
    public PipelinePublisher output;
    
    public slowtestplugin(JSONObject config) throws IOException, PluginException {
        super(config);

        postConstructorInit();

        loadState();
    }

    private void loadState() throws IOException
    {
        cachedValue = getSavedCache();
    }

    private String getSavedCache() throws IOException
    {
        Document state;
	
		state = getState();

        if (state != null && state.has(CachedValue))
        {
            return state.get(CachedValue).toString();
        }
        else
            return null;
    }
    
	private void saveValue(String cachedValue) throws IOException 
	{
		log.debug("Saving " + cachedValue);
		Document state;
		state = getState();
		
		if (state == null)
			state = new Document();
		
		state.put(CachedValue, cachedValue);
		
		setState(state);
	}
}
