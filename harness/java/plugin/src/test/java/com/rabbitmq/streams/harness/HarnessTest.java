package com.rabbitmq.streams.harness;

import static org.mockito.Mockito.*;

public class HarnessTest extends BaseTestCase {

  public void testObtainDefaultConfiguration()  {
    assertNotNull(getDefaultConfiguration());
  }

  public void testConstructor() {
    Harness harness = new Harness(getDefaultConfiguration());

    assertNotNull(harness);
  }
}
