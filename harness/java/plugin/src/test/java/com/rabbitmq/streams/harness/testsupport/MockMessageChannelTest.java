/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.rabbitmq.streams.harness.testsupport;

import com.rabbitmq.streams.harness.InputHandler;
import com.rabbitmq.streams.harness.InputMessage;
import net.sf.json.JSONObject;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.*;

import static org.mockito.Mockito.*;

/**
 *
 * @author mikeb
 */
public class MockMessageChannelTest {

    public MockMessageChannelTest() {
    }

  @BeforeClass
  public static void setUpClass() throws Exception {
  }

  @AfterClass
  public static void tearDownClass() throws Exception {
  }

  MockMessageChannel mmc;

    @Before
    public void setUp() {
      mmc = new MockMessageChannel();
    }

    @After
    public void tearDown() {
    }

  /**
   * Test of consume method, of class MockMessageChannel.
   */
  @Test
  public void testConsume() {
    InputHandler h = mock(InputHandler.class);
    mmc.consume("input", h);
    assertEquals(1, mmc.handlers.size());
    assertEquals(h, mmc.handlers.get("input"));
  }

  /**
   * Test of publish method, of class MockMessageChannel.
   */
  @Test
  public void testPublish() {
    InputMessage msg = mock(InputMessage.class);
    mmc.publish("output", msg);
    assertEquals(1, mmc.outputs.size());
    MockMessageChannel.Output out = mmc.outputs.get(0);
    assertEquals("output", out.channel);
    assertEquals(msg, out.msg);
  }

  /**
   * Test of inject method, of class MockMessageChannel.
   */
  @Test
  public void testInject() throws Exception {
    InputHandler h = mock(InputHandler.class);
    mmc.consume("input", h);
    InputMessage m = mock(InputMessage.class);
    mmc.inject("input", m);
    verify(h).handleMessage(eq(m), any(JSONObject.class));
    verifyNoMoreInteractions(h, m);
  }

}