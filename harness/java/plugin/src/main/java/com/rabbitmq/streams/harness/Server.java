package com.rabbitmq.streams.harness;

import com.rabbitmq.client.QueueingConsumer.Delivery;
import net.sf.json.JSONArray;
import net.sf.json.JSONObject;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 * A superclass for gateways. These have predefined inputs and
 * outputs, rather than having them specified, and don't enforce transactions.
 */
public abstract class Server extends Plugin {

  @Override
  public void configure(JSONObject staticConfig) {
    this.messageChannel.consume("command", command);
  }

  public void registerInputHandler(ServerInputReader handler) {
    this.messageChannel.consume("input", handler);
  }

  protected final void publishToDestination(byte[] body, String destination) throws IOException, MessagingException {
    this.messageChannel.publish("output", body, destination);
  }

  /**
   * Get all the configurations particular to this server
   *
   * @param terminalId the identifier for this terminal.
   * @return a list of JSONObject representing the configuration for this server.
   * @throws IOException if unable to get configuration documents from database.
   */
  protected final List<JSONObject> terminalConfigs(String terminalId) throws IOException {
    JSONObject wholeConfig = this.terminalsDatabase.getDocument(terminalId);
    JSONArray servers = wholeConfig.getJSONArray("servers");
    ArrayList<JSONObject> configs = new ArrayList<JSONObject>();
    for (int i = 0; i < servers.size(); i++) {
      JSONObject config = servers.getJSONObject(i);
      if (this.getId().equals(config.getString("server"))) {
        configs.add(config);
      }
    }
    return configs;
  }

  protected final JSONObject terminalStatus(String terminalId) throws IOException {
    return this.terminalsDatabase.getDocument(terminalId + "_status");
  }

  public static abstract class ServerInputReader implements InputHandler {

    @Override
    public void handleMessage(Message msg, JSONObject config) throws PluginException {
      handleBodyForTerminal(msg.body(), msg.routingKey(), msg);
    }

    abstract public void handleBodyForTerminal(byte[] body, String key, Message ack) throws PluginException;
  }

  private final InputHandler command = new InputHandler() {

    @Override
    public void handleMessage(Message message, JSONObject config) throws PluginException {

      String serverIdterminalId = message.routingKey();
      int loc = serverIdterminalId.lastIndexOf('.');
      String serverIds = serverIdterminalId.substring(0, loc);
      String terminalId = serverIdterminalId.substring(loc + 1);

      try {
        List<JSONObject> terminalConfigs = Server.this.terminalConfigs(terminalId);
        JSONObject terminalStatus = Server.this.terminalStatus(terminalId);

        if (!serverIds.contains(Server.this.getId())) {
          Server.this.log.error(
            "Received a terminal status change " +
              "message which was not routed for us: " +
              serverIds);
          return;
        }

        if (terminalConfigs.size() == 0) {
          Server.this.log.error(
            "Received a terminal status change "
              + "message for a terminal which isn't "
              + "configured for us: " + terminalConfigs);
          return;
        }

        Server.this.log.info(
          "Received terminal status change for " + terminalId);

        Server.this.terminalStatusChange(terminalId,
          terminalConfigs,
          terminalStatus.getBoolean("active"));
        try {
          message.ack();
        }
        catch (MessagingException me) {
          log.error("Unable to ack message");
          // FIXME rollback?
        }
      }
      catch (IOException ex) {
        throw new PluginException("Unable to handle delivery.", ex);
      }
    }
  };

  /**
   * Handle a status change.  This will be called from another thread, so take care.
   * <p/>
   * QUESTION - wouldn't this be named better as changeTerminalStatus?
   *
   * @param terminalId the identifier for this terminal.
   * @param configs    the new configurations for the terminal.
   * @param active     the status of the terminal.
   */
  protected abstract void terminalStatusChange(String terminalId, List<JSONObject> configs, boolean active);
}
