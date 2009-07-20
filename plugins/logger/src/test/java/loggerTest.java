/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

import com.rabbitmq.streams.harness.InputHandler;
import com.rabbitmq.streams.harness.InputMessage;
import com.rabbitmq.streams.harness.Logger;
import com.rabbitmq.streams.harness.Message;
import com.rabbitmq.streams.harness.MessageChannel;
import com.rabbitmq.streams.harness.MessagingException;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

import net.sf.json.JSONObject;

/**
 *
 * @author mikeb
 */
public class loggerTest {

    public loggerTest() {
    }

  @BeforeClass
  public static void setUpClass() throws Exception {
  }

  @AfterClass
  public static void tearDownClass() throws Exception {
  }

    @Before
    public void setUp() {
    }

    @After
    public void tearDown() {
    }

  /**
   * Test of configure method, of class logger.
   */
  @Test
  public void testConfigure() {
    System.out.println("configure");
    JSONObject config = null;
    logger instance = new logger();
    instance.configure(config);
    // TODO review the generated test code and remove the default call to fail.
    fail("The test case is a prototype.");
  }

  @Test
  public void testHandleMessage() throws Exception {
    logger log = new logger();
    JSONObject config = new JSONObject();
    Logger l = mock(Logger.class);
    log.setLog(l);
    Injector handlerBox = new Injector();
    log.setMessageChannel(handlerBox);
    log.configure(config);
    InputMessage m = mock(InputMessage.class);
    when(m.body()).thenReturn("hello".getBytes());
    handlerBox.handlers.get("input").handleMessage(m, new JSONObject());
    verify(l).debug("hello");
  }

  class Injector implements MessageChannel {
    public Map<String, InputHandler> handlers = new HashMap();

    public void consume(String channelName, InputHandler handler) {
      handlers.put(channelName, handler);
    }

    public void publish(String channelName, Message msg) throws IOException, MessagingException {
      throw new UnsupportedOperationException("Not supported yet.");
    }

  }

}