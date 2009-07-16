package com.rabbitmq.streams.harness;

import net.sf.json.JSONArray;
import net.sf.json.JSONObject;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

public abstract class Plugin {

  public static final String newline = System.getProperty("line.separator");

  protected String id;
  private StateResource stateResource;
  protected Logger log;
  protected DatabaseResource privateDb;
  protected DatabaseResource terminalsDatabase;

  protected JSONObject pluginType;
  protected JSONObject staticConfiguration;

  private final Map<String, Publisher> outputs = new HashMap<String, Publisher>();
  private final Map<String, InputHandler> handlers = new HashMap<String, InputHandler>();
  protected MessageChannel messageChannel;

  public void configure(final JSONObject staticConfig) {
    staticConfiguration = staticConfig;
  }

  protected void setId(String id) {
    this.id = id;
  }

  public String getId() {
    return id;
  }

  public boolean configuredCorrectly()  {
    return true;
  }

  void setStateResource(StateResource state) {
    this.stateResource = state;
  }

  void setTerminalsDatabase(DatabaseResource db) {
    this.terminalsDatabase = db;
  }

  void setMessageChannel(MessageChannel channel) {
    this.messageChannel = channel;
  }

  /**
   * For plugins to set their state
   * @param name
   * @return
   */
  protected final void setState(Map<String, Object> state) {
    try {
      stateResource.setState(state);
    }
    catch (IOException ioe) {

    }
  }

  /**
   * For plugins to get their state
   * @return
   */
  protected final Map<String, Object> getState() {
    try {
      return stateResource.getState();
    }
    catch (IOException ioe) {
      log.fatal("Cannot read state");
      this.dieHorribly();
      return null; // obey the type system
    }
  }

  InputHandler handler(String name)  {
    return handlers.get(name);
  }

  public void setLog(Logger log) {
    this.log = log;
  }

  public void setDatabase(DatabaseResource database) {
    privateDb = database;
  }

  protected final void dieHorribly() {
    System.exit(1);
  }

}
