package com.rabbitmq.streams.plugins.socket.destination;
import com.rabbitmq.streams.harness.InputMessage;
import com.rabbitmq.streams.harness.PluginBuildException;
import com.rabbitmq.streams.harness.Server;
import com.rabbitmq.streams.harness.PluginException;
import net.sf.json.JSONObject;

import java.io.IOException;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.Socket;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

public class SocketDestinationServer extends Server {

  private final Map<String, List<SocketDestination>> terminalMap = new HashMap<String, List<SocketDestination>>();

  public final Server.ServerInputReader input = new Server.ServerInputReader() {

    @Override
    public void handleBodyForTerminal(byte[] body, String key, InputMessage ack) throws PluginException {
      List<SocketDestination> dests = terminalMap.get(key);
      if (null != dests) {
        for (SocketDestination dest : dests) {
          dest.send(body);
        }
      }
      try {
        ack.ack();
      }
      catch (Exception e) {
        throw new PluginException(e);
      }
    }
  };

  protected void terminalStatusChange(String terminalId, List<JSONObject> terminalConfigs, boolean active) {
    if (active) {
      List<SocketDestination> dests = terminalMap.get(terminalId);
      if (null == dests || 0 == dests.size()) {
        try {
          dests = new ArrayList<SocketDestination>(terminalConfigs.size());
          for (JSONObject termConfigObject : terminalConfigs) {
            JSONObject destConfig = termConfigObject.getJSONObject("destination");
            SocketDestination dest = new SocketDestination(destConfig);
            dests.add(dest);
            new Thread(dest).start();
          }
          terminalMap.put(terminalId, dests);
        }
        catch (IOException e) {
          SocketDestinationServer.this.log.error(e);
        }
      }
    }
    else {
      List<SocketDestination> dests = terminalMap.remove(terminalId);
      if (null != dests && 0 != dests.size()) {
        for (SocketDestination dest : dests) {
          dest.stop();
        }
      }
    }
  }

  @Override
  public void configure(JSONObject config) throws PluginBuildException {
    super.configure(config);
    registerInput(input);
  }

  private final class SocketDestination implements Runnable {
    final private int port;
    final private InetAddress address;
    final private Socket socket;
    final private OutputStream socketOutputStream;
    final private BlockingQueue<byte[]> sendQueue = new LinkedBlockingQueue<byte[]>();

    final private Object lockObj = new Object();
    private boolean running = true;

    public SocketDestination(JSONObject terminalConfig) throws IOException {
      port = terminalConfig.getInt("port");
      address = InetAddress.getByName(terminalConfig.getString("host"));
      socket = new Socket(address, port);
      socketOutputStream = socket.getOutputStream();
    }

    private boolean isRunning() {
      synchronized (lockObj) {
        return running;
      }
    }

    public void send(byte[] msg) {
      try {
        sendQueue.put(msg);
      }
      catch (InterruptedException e) {
        send(msg);
      }
    }

    public void run() {
      while (isRunning()) {
        try {
          socketOutputStream.write(sendQueue.take());
          System.out.println("WRITTEN TO SOCKET");
        }
        catch (InterruptedException ignore) {
        }
        catch (IOException e) {
          SocketDestinationServer.this.log.error(e);
        }
      }
      try {
        socket.close();
      }
      catch (IOException e) {
        SocketDestinationServer.this.log.error(e);
      }
    }

    public void stop() {
      synchronized (lockObj) {
        running = false;
      }
    }
  }

}

