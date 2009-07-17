/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.rabbitmq.streams.harness;

import java.util.HashMap;
import java.util.Map;
import net.sf.json.JSONObject;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.*;


/**
 *
 * @author mikeb
 */
public class PluginTest extends PluginTestingTools {

  public PluginTest() {
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
   * Test of configuredCorrectly method, of class Plugin.
   */
  //@Test
  public void testConfiguredCorrectly() {
    // TODO test base cases of incorrect config
  }

  /**
   * Test of setValuesInHeader method, of class Plugin.
   */
  @Test
  public void testSetValuesInHeader() {
    Map<String, Object> header = new HashMap();
    JSONObject vals = new JSONObject();
    vals.put("foo", "bar");
    AMQPInputConsumer.setValuesInHeader(header, vals);
    assertEquals(AMQPInputConsumer.getValuesFromHeader(header), vals);
    assertEquals(AMQPInputConsumer.getValuesFromHeader(header).getString("foo"), "bar");
  }

  /**
   * Test of configure method, of class Plugin.
   */
  @Test
  public void testConfigure() {
    System.out.println("configure");
    JSONObject staticConfig = null;
    Plugin instance = new PluginImpl();
    instance.configure(staticConfig);
    // TODO review the generated test code and remove the default call to fail.
    fail("The test case is a prototype.");
  }

  /**
   * Test of getId method, of class Plugin.
   */
  @Test
  public void testGetId() {
    System.out.println("getId");
    Plugin instance = new PluginImpl();
    String expResult = "";
    String result = instance.getId();
    assertEquals(expResult, result);
    // TODO review the generated test code and remove the default call to fail.
    fail("The test case is a prototype.");
  }

  /**
   * Test of setId method, of class Plugin.
   */
  @Test
  public void testSetId() {
    System.out.println("setId");
    String id = "";
    Plugin instance = new PluginImpl();
    instance.setId(id);
    // TODO review the generated test code and remove the default call to fail.
    fail("The test case is a prototype.");
  }

  /**
   * Test of setStateResource method, of class Plugin.
   */
  @Test
  public void testSetStateResource() {
    System.out.println("setStateResource");
    StateResource state = null;
    Plugin instance = new PluginImpl();
    instance.setStateResource(state);
    // TODO review the generated test code and remove the default call to fail.
    fail("The test case is a prototype.");
  }

  /**
   * Test of setTerminalsDatabase method, of class Plugin.
   */
  @Test
  public void testSetTerminalsDatabase() {
    System.out.println("setTerminalsDatabase");
    DatabaseResource db = null;
    Plugin instance = new PluginImpl();
    instance.setTerminalsDatabase(db);
    // TODO review the generated test code and remove the default call to fail.
    fail("The test case is a prototype.");
  }

  /**
   * Test of setMessageChannel method, of class Plugin.
   */
  @Test
  public void testSetMessageChannel() {
    System.out.println("setMessageChannel");
    MessageChannel channel = null;
    Plugin instance = new PluginImpl();
    instance.setMessageChannel(channel);
    // TODO review the generated test code and remove the default call to fail.
    fail("The test case is a prototype.");
  }

  /**
   * Test of setLog method, of class Plugin.
   */
  @Test
  public void testSetLog() {
    System.out.println("setLog");
    Logger log = null;
    Plugin instance = new PluginImpl();
    instance.setLog(log);
    // TODO review the generated test code and remove the default call to fail.
    fail("The test case is a prototype.");
  }

  /**
   * Test of setDatabase method, of class Plugin.
   */
  @Test
  public void testSetDatabase() {
    System.out.println("setDatabase");
    DatabaseResource database = null;
    Plugin instance = new PluginImpl();
    instance.setDatabase(database);
    // TODO review the generated test code and remove the default call to fail.
    fail("The test case is a prototype.");
  }

  /**
   * Test of setState method, of class Plugin.
   */
  @Test
  public void testSetState() {
    System.out.println("setState");
    Map<String, Object> state = null;
    Plugin instance = new PluginImpl();
    instance.setState(state);
    // TODO review the generated test code and remove the default call to fail.
    fail("The test case is a prototype.");
  }

  /**
   * Test of getState method, of class Plugin.
   */
  @Test
  public void testGetState() {
    System.out.println("getState");
    Plugin instance = new PluginImpl();
    Map expResult = null;
    Map result = instance.getState();
    assertEquals(expResult, result);
    // TODO review the generated test code and remove the default call to fail.
    fail("The test case is a prototype.");
  }

  /**
   * Test of dieHorribly method, of class Plugin.
   */
  @Test
  public void testDieHorribly() {
    System.out.println("dieHorribly");
    Plugin instance = new PluginImpl();
    instance.dieHorribly();
    // TODO review the generated test code and remove the default call to fail.
    fail("The test case is a prototype.");
  }

  public class PluginImpl extends Plugin {

    public void configure(JSONObject staticConfig) {
    }
  }

}
