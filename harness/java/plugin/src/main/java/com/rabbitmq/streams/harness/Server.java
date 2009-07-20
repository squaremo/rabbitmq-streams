package com.rabbitmq.streams.harness;

import com.fourspaces.couchdb.Database;
import com.fourspaces.couchdb.Document;
import com.fourspaces.couchdb.Session;
import com.rabbitmq.client.QueueingConsumer.Delivery;
import net.sf.json.JSONArray;
import net.sf.json.JSONObject;

import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

/**
 * A superclass for gateways (ho ho). These have predefined inputs and
 * outputs, rather than having them specified, and don't enforce transactions.
 */
public abstract class Server extends Plugin {
  final protected Database terminalsDatabase;
  final protected String serverId;

  public Server(JSONObject config) throws IOException {
    super(config);
    this.serverId = config.getString("server_id");
    String terminalsDbStr = config.getString("terminals_database");
    URL terminalsDbUrl = new URL(terminalsDbStr);

    Session couchSession = new Session(terminalsDbUrl.getHost(), terminalsDbUrl.getPort(), "", "");
    String path = terminalsDbUrl.getPath();
    int loc;
    if (path.endsWith("/")) {
      loc = path.substring(0, path.length() - 1).lastIndexOf('/');
    }
    else {
      loc = path.lastIndexOf('/');
    }
    String terminalsDbName = path.substring(loc);
    terminalsDatabase = couchSession.getDatabase(terminalsDbName);

    registerHandler("command", command);
  }

  @Override
  public InputReaderRunnable handlerRunnable(String name) {
    return new DefaultInputReaderRunnable();
  }

  protected final void publishToDestination(byte[] body, String destination) throws IOException {
    getPublisher("output").publish(body, destination);
  }

  /**
   * Get all the configurations particular to this server
   *
   * @param terminalId the identifier for this terminal.
   * @return a list of JSONObject representing the configuration for this server.
   * @throws IOException if unable to get configuration documents from database.
   */
  protected final List<JSONObject> terminalConfigs(String terminalId) throws IOException {
    Document wholeConfig = this.terminalsDatabase.getDocument(terminalId);
    JSONArray servers = wholeConfig.getJSONArray("servers");
    ArrayList<JSONObject> configs = new ArrayList<JSONObject>();
    for (int i = 0; i < servers.size(); i++) {
      JSONObject config = servers.getJSONObject(i);
      if (this.serverId.equals(config.getString("server"))) {
        configs.add(config);
      }
    }
    return configs;
  }

  protected final Document terminalStatus(String terminalId) throws IOException {
    return this.terminalsDatabase.getDocument(terminalId + "_status");
  }

  public static abstract class ServerInputReader implements InputHandler {

    public void handleDelivery(Delivery delivery, JSONObject config) throws PluginException {
      handleBodyForTerminal(delivery.getBody(), delivery.getEnvelope().getRoutingKey(), delivery.getEnvelope().getDeliveryTag());
    }

    abstract public void handleBodyForTerminal(byte[] body, String key, long tagToAck) throws PluginException;
  }

  private final InputHandler command = new InputHandler() {

    public void handleDelivery(Delivery delivery, JSONObject config) throws PluginException {

      String serverIdterminalId = delivery.getEnvelope().getRoutingKey();
      int loc = serverIdterminalId.lastIndexOf('.');
      String serverIds = serverIdterminalId.substring(0, loc);
      String terminalId = serverIdterminalId.substring(loc + 1);

      try {
        List<JSONObject> terminalConfigs = Server.this.terminalConfigs(terminalId);
        Document terminalStatus = Server.this.terminalStatus(terminalId);

        if (!serverIds.contains(Server.this.serverId)) {
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
        Server.this.ack(delivery);
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
