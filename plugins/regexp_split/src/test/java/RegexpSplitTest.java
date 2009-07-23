import org.junit.Test;
import org.junit.Before;
import static org.mockito.Mockito.*;
import static org.junit.Assert.*;
import com.rabbitmq.streams.harness.PluginBuildException;
import com.rabbitmq.streams.harness.MessageChannel;
import net.sf.json.JSONObject;

public class RegexpSplitTest {
  private regexp_split splitter;
  private JSONObject config = new JSONObject();

  @Before
  public void setup() {
    config.put("regexp", "s");
    config.put("multiline", true);
    config.put("caseinsensitive", true);
    config.put("dotall", true);
    splitter = new regexp_split();
  }

  @Test
  public void testConfigureWithNullConfig() {
    try {
      splitter.configure(null);
      fail("Cannot configure a plugin with a null configuration");
    }
    catch (PluginBuildException ignore) {
    }
  }

  @Test
  public void testConfigureWithNoRegexp() {

  }

  @Test
  public void testConfigure() {
    MessageChannel channel = mock(MessageChannel.class);
    splitter.setMessageChannel(channel);
    try {
      splitter.configure(config);
    }
    catch (PluginBuildException e) {
      e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
    }
  }
}
