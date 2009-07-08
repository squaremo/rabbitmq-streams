package com.rabbitmq.streams.harness;

import static org.mockito.Mockito.*;

public class HarnessTest extends BaseTestCase {

  public void testObtainDefaultConfiguration()  {
    assertNotNull(getDefaultConfiguration());
  }

  public void testConstructor() {
    Plugin plugin = mock(Plugin.class);

    Harness harness = new Harness(plugin);

    assertNotNull(harness);
  }
}
