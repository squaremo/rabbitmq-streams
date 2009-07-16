package com.rabbitmq.streams.harness;

import net.sf.json.JSONObject;
import org.junit.Test;
import static org.mockito.Mockito.*;

public class HarnessTest extends BaseTestCase {

  public void testConstructor() {
    Logger log = mock(Logger.class);
    PluginBuilder pb = new PluginBuilder(log);
    assertNotNull(pb);
  }

}
