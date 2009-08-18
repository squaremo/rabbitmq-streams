/*
 */

package com.rabbitmq.streams.harness;

import com.rabbitmq.streams.harness.Server.ServerInputReader;
import java.util.List;
import java.util.Map;
import net.sf.json.JSONArray;
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
public class ServerTest {

    public ServerTest() {
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
   * Test of configure method, of class Server.
   */
  //@Test // not sure what this should test
  public void testConfigure() throws Exception {
    System.out.println("configure");
    JSONObject staticConfig = null;
    Server instance = new ServerImpl();
    instance.configure(staticConfig);
    // TODO review the generated test code and remove the default call to fail.
    fail("The test case is a prototype.");
  }

  /**
   * Test that a registered input handler will get messages sent to
   * the "input" channel.
   */
  @Test
  public void testRegisterInput() throws Exception {
    final ServerInputReader h = mock(ServerInputReader.class);
    InputMessage msg = mock(InputMessage.class);
    when(msg.body()).thenReturn("Hello".getBytes("utf-8"));
    when(msg.routingKey()).thenReturn("testTerminal");
    Server s = new Server() {
      @Override public void configure(JSONObject config) throws PluginBuildException {
        super.configure(config);
        registerInput(h);
      }

      @Override
      protected void terminalStatusChange(String terminalId, List<JSONObject> configs, boolean active) {
        throw new UnsupportedOperationException("Not supported yet.");
      }
    };
    TestServerMessageChannel mc = new TestServerMessageChannel();
    s.setMessageChannel(mc);
    s.configure(new JSONObject());
    assertNotNull(mc.input);
    mc.input.handleMessage(msg, new JSONObject());
    verify(h).handleMessage(msg, new JSONObject());
  }

  /**
   * Test of publishToDestination method, of class Server.
   */
  //@Test
  public void testPublishToDestination_byteArr_String() throws Exception {
    System.out.println("publishToDestination");
    byte[] body = null;
    String destination = "";
    Server instance = new ServerImpl();
    instance.publishToDestination(body, destination);
    // TODO review the generated test code and remove the default call to fail.
    fail("The test case is a prototype.");
  }

  /**
   * Test of publishToDestination method, of class Server.
   */
  //@Test
  public void testPublishToDestination_3args() throws Exception {
    System.out.println("publishToDestination");
    byte[] body = null;
    String destination = "";
    Map<String, Object> headers = null;
    Server instance = new ServerImpl();
    instance.publishToDestination(body, destination, headers);
    // TODO review the generated test code and remove the default call to fail.
    fail("The test case is a prototype.");
  }

  /**
   * Test of terminalConfigs method, of class Server.
   */
  //@Test
  public void testTerminalConfigs() throws Exception {
    System.out.println("terminalConfigs");
    String terminalId = "";
    Server instance = new ServerImpl();
    List expResult = null;
    List result = instance.terminalConfigs(terminalId);
    assertEquals(expResult, result);
    // TODO review the generated test code and remove the default call to fail.
    fail("The test case is a prototype.");
  }

  /**
   * Test of terminalStatus method, of class Server.
   */
  //@Test
  public void testTerminalStatus() throws Exception {
    System.out.println("terminalStatus");
    String terminalId = "";
    Server instance = new ServerImpl();
    JSONObject expResult = null;
    JSONObject result = instance.terminalStatus(terminalId);
    assertEquals(expResult, result);
    // TODO review the generated test code and remove the default call to fail.
    fail("The test case is a prototype.");
  }

  /**
   * Test that terminalStatusChange gets invoked when an appropriate message is
   * sent along the command channel.
   */
  @Test
  public void testTerminalStatusChange() throws Exception {
    final String[] terminal = {"wrong"};
    final boolean[] active = {false};

    Server s = new ServerImpl() {
      @Override
      protected void terminalStatusChange(String t, List<JSONObject> s, boolean a) {
        active[0] = a; terminal[0] = t;
      }
    };
    TestServerMessageChannel mc = new TestServerMessageChannel();
    s.setId("server");
    s.setMessageChannel(mc);
    s.setLog(mock(Logger.class));
    JSONObject termStatus = new JSONObject();
    termStatus.put("type", "terminal-status");
    termStatus.put("active", true);
    JSONObject termConfig = new JSONObject();
    termConfig.put("type", "terminal");
    JSONArray servers = new JSONArray();
    JSONObject server = new JSONObject();
    server.put("server", "server");
    servers.add(server);
    termConfig.put("servers", servers);
    DatabaseResource db = mock(DatabaseResource.class);
    when(db.getDocument("terminal")).thenReturn(termConfig);
    when(db.getDocument("terminal_status")).thenReturn(termStatus);
    s.setTerminalsDatabase(db);
    List<JSONObject> termConfigs = JSONArray.toList(servers);
    InputMessage msg = mock(InputMessage.class);
    when(msg.body()).thenReturn("status change".getBytes());
    // We use routing keys with the servers involved and terminal
    when(msg.routingKey()).thenReturn("server.terminal");
    s.configure(new JSONObject());
    mc.command.handleMessage(msg, new JSONObject());
    assertEquals("terminal", terminal[0]);
    assertEquals(true, active[0]);
    verify(db).getDocument("terminal");
    verify(db).getDocument("terminal_status");
    verify(msg).ack();
  }

  public class ServerImpl extends Server {

    @SuppressWarnings("empty-statement")
    @Override
    protected void terminalStatusChange(String terminalId, List<JSONObject> configs, boolean active) {
      ;
    }
  }

  public class TestServerMessageChannel implements MessageChannel {
      public InputHandler input;
      public InputHandler command;

      public void consume(String channelName, InputHandler handler) throws MessagingException {
        if ("input".equals(channelName)) {
          input = handler;
        }
        else if ("command".equals(channelName)) {
          command = handler;
        }
        else throw new MessagingException("Unknown channel " + channelName);
      }

      public void publish(String channelName, Message msg) throws MessagingException {
        throw new UnsupportedOperationException("Not supported yet.");
      }

    }

}