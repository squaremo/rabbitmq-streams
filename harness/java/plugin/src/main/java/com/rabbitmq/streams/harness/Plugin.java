package com.rabbitmq.streams.harness;

import net.sf.json.JSONObject;

import java.io.IOException;
import java.util.Map;

public abstract class Plugin {

  public static final String newline = System.getProperty("line.separator");

  protected String id;
  protected StateResource stateResource;
  protected Logger log;
  private DatabaseResource privateDb;
  protected DatabaseResource terminalsDatabase;
  protected Notifier notifier;

  protected MessageChannel messageChannel;

  public abstract void configure(final JSONObject staticConfig) throws PluginBuildException;
  
  public String getId() {
    return id;
  }

  public boolean configuredCorrectly()  {
    return true;
  }

// <editor-fold defaultstate="collapsed" desc="Mutators for the harness">
  public void setId(String id) {
    this.id = id;
  }

  public void setStateResource(StateResource state) {
    this.stateResource = state;
  }

  public void setTerminalsDatabase(DatabaseResource db) {
    this.terminalsDatabase = db;
  }

  public void setMessageChannel(MessageChannel channel) {
    this.messageChannel = channel;
  }

  public void setLog(Logger log) {
    this.log = log;
  }

  public void setNotifier(Notifier notifier) {
    this.notifier = notifier;
  }

  public DatabaseResource getDatabase() {
    return privateDb;
  }

  public void setDatabase(DatabaseResource database) {
    privateDb = database;
  }// </editor-fold>

  /**
   * For plugins to set their state
   * @param state
   * @return
   */
  protected final void setState(Map<String, Object> state) {
    try {
      stateResource.setState(state);
    }
    catch (IOException ignored) {

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

  protected final void notification(NotificationType kind, InputMessage msg) {
    notifier.notify(kind, msg.bodyAsStringLax(), msg.headers());
  }

  protected final void notification(NotificationType kind, String msg) {
    notifier.notify(kind, msg);
  }

  protected final void dieHorribly() {
    System.exit(1);
  }

}
