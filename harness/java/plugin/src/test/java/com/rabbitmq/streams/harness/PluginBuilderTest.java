/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.rabbitmq.streams.harness;

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
public class PluginBuilderTest {
  public static final String TEST_STATE_URL = "http://example.com/database/state";
  private static final String TEST_DB_URL = "http://example.com/database";
  private Logger log;
  private PluginResourceFactory factory;
  private static JSONObject pluginConfig;

  public PluginBuilderTest() {
  }

  @BeforeClass
  public static void setUpClass() throws Exception {
  }

  @AfterClass
  public static void tearDownClass() throws Exception {
  }

  @Before
  public void setUp() {
    log = mock(Logger.class);
    factory = mock(PluginResourceFactory.class);
  }

    @After
    public void tearDown() {
    }

  /**
   * Test of buildPlugin method, of class PluginBuilder.
   */
  //@Test Template-generated, disabled for now
  public void testBuildPlugin() {
    System.out.println("buildPlugin");
    JSONObject configuration = null;
    PluginBuilder instance = null;
    Plugin expResult = null;
    Plugin result = instance.buildPlugin(configuration);
    assertEquals(expResult, result);
    // TODO review the generated test code and remove the default call to fail.
    fail("The test case is a prototype.");
  }

  /**
   * Test of configurePlugin method, of class PluginBuilder.
   */
  //@Test Template-generated, disabled for now
  public void testConfigurePlugin() throws Exception {
    System.out.println("configurePlugin");
    Plugin plugin = null;
    JSONObject configuration = null;
    PluginBuilder instance = null;
    instance.configurePlugin(plugin, configuration);
    // TODO review the generated test code and remove the default call to fail.
    fail("The test case is a prototype.");
  }

  /**
   * Test of mergedStaticConfiguration method, of class PluginBuilder.
   */
  //@Test Template-generated, disabled for now
  public void testMergedStaticConfiguration() {
    System.out.println("mergedStaticConfiguration");
    JSONObject configuration = null;
    JSONObject expResult = null;
    JSONObject result = PluginBuilder.mergedStaticConfiguration(configuration);
    assertEquals(expResult, result);
    // TODO review the generated test code and remove the default call to fail.
    fail("The test case is a prototype.");
  }

  /**
   * Test of configureServer method, of class PluginBuilder.
   */
  //@Test Template-generated, disabled for now
  public void testConfigureServer() throws Exception {
    Server plugin = mock(Server.class);
    JSONObject configuration = new JSONObject();
    PluginBuilder instance = null;
    instance.configureServer(plugin, configuration);
    // TODO review the generated test code and remove the default call to fail.
    fail("The test case is a prototype.");
  }

  /**
   * Test of configurePipelineComponent method, of class PluginBuilder.
   */
  //@Test Template-generated, disabled for now
  public void testConfigurePipelineComponent() throws Exception {
    System.out.println("configurePipelineComponent");
    Plugin plugin = null;
    JSONObject configuration = null;
    PluginBuilder instance = null;
    instance.configurePipelineComponent(plugin, configuration);
    // TODO review the generated test code and remove the default call to fail.
    fail("The test case is a prototype.");
  }

  /**
   * Test of setStateResourceOnPlugin method, of class PluginBuilder.
   */
  @Test
  public void testSetStateResourceOnPlugin() throws Exception {
    JSONObject c = new JSONObject();
    c.put("state",TEST_STATE_URL);
    Plugin plugin = mock(Plugin.class);
    PluginBuilder instance = new PluginBuilder(log, factory);
    StateResource res = mock(StateResource.class);
    when(factory.getStateResource(TEST_STATE_URL)).thenReturn(res);
    instance.setStateResourceOnPlugin(c, plugin);
    verify(factory).getStateResource(TEST_STATE_URL);
    verify(plugin).setStateResource(res);
  }

  /**
   * Test of connectDatabaseToPlugin method, of class PluginBuilder.
   */
  @Test
  public void testConnectDatabaseToPlugin() throws Exception {
    JSONObject dbconfig = new JSONObject();
    dbconfig.put("database",TEST_DB_URL);
    PluginBuilder pb = new PluginBuilder(log, factory);
    Plugin p = mock(Plugin.class);
    DatabaseResource db = mock(DatabaseResource.class);
    when(factory.getDatabase(TEST_DB_URL)).thenReturn(db);
    pb.connectDatabaseToPlugin(p, dbconfig);
    verify(p).setDatabase(db);
    verify(factory).getDatabase(TEST_DB_URL);
    verifyNoMoreInteractions(p, factory);
  }

  /**
   * Test of configurationError method, of class PluginBuilder.
   * TODO I don't know if we should be checking this.
   */
  @Test
  public void testConfigurationError() {
    PluginBuilder pb = new PluginBuilder(log, factory);
    Exception ex = new Exception("Test exception");
    pb.configurationError("Test message", ex);
    verify(log).error(anyString());
  }

  /**
   * Test of constructPlugin method, of class PluginBuilder.
   */
  @Test
  public void testConstructPlugin() throws Exception {
    ClassLoader cloader = this.getClass().getClassLoader();
    String pluginName = "com.rabbitmq.streams.harness.PluginBuilderTest$TestPlugin";
    PluginBuilder instance = new PluginBuilder(log, factory);
    Plugin result = instance.constructPlugin(cloader, pluginName);
    assertNotNull(result);
    assertTrue(result instanceof TestPlugin);
  }

  @Test(expected = PluginNotFoundException.class)
  public void testConstructPluginNotFound() throws Exception {
    ClassLoader cloader = this.getClass().getClassLoader();
    String pluginName = "DOESNOTEXIST";
    PluginBuilder instance = new PluginBuilder(log, factory);
    instance.constructPlugin(cloader, pluginName);
  }

  @Test(expected = PluginNotFoundException.class)
  public void testConstructPluginNoConstructor() throws Exception {
    ClassLoader cloader = this.getClass().getClassLoader();
    String pluginName = "com.rabbitmq.streams.harness.PluginBuilderTest$NoDefaultConstructorPlugin";
    PluginBuilder instance = new PluginBuilder(log, factory);
    instance.constructPlugin(cloader, pluginName);
  }

  public static class NoDefaultConstructorPlugin extends Plugin {

    private NoDefaultConstructorPlugin() {}

    @Override
    public void configure(JSONObject staticConfig) {
      throw new UnsupportedOperationException("Not supported yet.");
    }

  }

  public static class TestPlugin extends Plugin {

    @Override
    public void configure(JSONObject staticConfig) {
      throw new UnsupportedOperationException("Not supported yet.");
    }

  }

}