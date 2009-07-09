
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import net.sf.json.JSONObject;

import com.fourspaces.couchdb.Document;
import com.rabbitmq.streams.harness.InputReader;
import com.rabbitmq.streams.harness.PipelineComponent;
import com.rabbitmq.streams.harness.PluginException;


public class onevaluecachejava extends PipelineComponent {

    private String cachedValue = null;
    
    public final InputReader cache = new InputReader() {

        public void handleBody(byte[] body) throws PluginException {
            cachedValue = new String(body);
            
            try {
				Document bob = getState();
				String hoh;
				
				
								
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
            
        }

    };

    public final InputReader input = new InputReader() {

        public void handleBody(byte[] body) throws PluginException {
            Map headers = new HashMap();
            
            if (cachedValue != null)
            {
                //Check message against cachedValue
                headers.put("cache2", cachedValue);
            }
            
            //Send on message possibly modified
            try {
                output.publish(body, headers);
            } catch (IOException e) {
                throw new PluginException(e);
            }
        }

    };
    
    public PipelinePublisher output;
    
    public onevaluecachejava(JSONObject config) throws IOException {
        super(config);

        postConstructorInit();
    }


}
