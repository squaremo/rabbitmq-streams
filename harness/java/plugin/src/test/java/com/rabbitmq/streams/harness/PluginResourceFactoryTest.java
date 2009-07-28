/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.rabbitmq.streams.harness;

import com.fourspaces.couchdb.Database;
import com.fourspaces.couchdb.Session;
import com.rabbitmq.client.Connection;
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
public class PluginResourceFactoryTest {
  private Connection connection;
  private SessionFactory sessionFactory;
  private Logger log;

  public PluginResourceFactoryTest() {
  }

  @BeforeClass
  public static void setUpClass() throws Exception {
  }

  @AfterClass
  public static void tearDownClass() throws Exception {
  }

    @Before
    public void setUp() {
      connection = mock(Connection.class);
      sessionFactory = mock(SessionFactory.class);
      log = mock(Logger.class);
    }

    @After
    public void tearDown() {
    }

  /**
   * Test of getDatabase method, of class PluginResourceFactory.
   */
  @Test
  public void testGetDatabase() throws Exception {
    final String dbUrl = "http://localhost:9999/database";
    PluginResourceFactory pf = new PluginResourceFactory(connection, sessionFactory, log);
    Session s = mock(Session.class);
    Database d = mock(Database.class);
    when(d.getName()).thenReturn("database");
    when(s.getDatabase("database")).thenReturn(d);
    when(sessionFactory.createSession(anyString(), anyInt(), anyString(), anyString())).thenReturn(s);
    DatabaseResource db = pf.getDatabase(dbUrl);
    assertNotNull(db);
    verify(s).getDatabase("database");
    assertEquals("database", db.getName());
  }

  @Test
  public void testNonexistantDocument() throws Exception {
    Session s = mock(Session.class);
    Database d = mock(Database.class);
    when(d.getDocument(anyString())).thenReturn(null); // null == doesn't exist
    when(s.getDatabase(anyString())).thenReturn(d);
    when(sessionFactory.createSession(anyString(), anyInt(), anyString(), anyString())).thenReturn(s);
    PluginResourceFactory pf = new PluginResourceFactory(connection, sessionFactory, log);
    DatabaseResource db = pf.getDatabase("http://example.com/");
    assertNull(db.getDocument("anything"));
  }

  @Test
  public void testGetDatabaseTrailingSlash() throws Exception {
    final String dbUrl = "http://localhost:9999/database/";
    Session s = mock(Session.class);
    Database d = mock(Database.class);
    when(d.getName()).thenReturn("database");
    when(s.getDatabase("database")).thenReturn(d);
    when(sessionFactory.createSession(anyString(), anyInt(), anyString(), anyString())).thenReturn(s);
    PluginResourceFactory pf = new PluginResourceFactory(connection, sessionFactory, log);
    DatabaseResource db = pf.getDatabase(dbUrl);
    assertNotNull(db);
    verify(s).getDatabase("database");
    assertEquals("database", db.getName());
  }

  /**
   * Test of getMessageResource method, of class PluginResourceFactory.
   */
  //@Test
  public void testGetMessageResource() throws Exception {
    System.out.println("getMessageResource");
    JSONObject config = null;
    PluginResourceFactory instance = null;
    MessageResource expResult = null;
    MessageResource result = instance.getMessageResource(config);
    assertEquals(expResult, result);
    // TODO review the generated test code and remove the default call to fail.
    fail("The test case is a prototype.");
  }

  /**
   * Test of getStateResource method, of class PluginResourceFactory.
   */
  //@Test
  public void testGetStateResource() throws Exception {
    System.out.println("getStateResource");
    String stateString = "";
    PluginResourceFactory instance = null;
    StateResource expResult = null;
    StateResource result = instance.getStateResource(stateString);
    assertEquals(expResult, result);
    // TODO review the generated test code and remove the default call to fail.
    fail("The test case is a prototype.");
  }

}