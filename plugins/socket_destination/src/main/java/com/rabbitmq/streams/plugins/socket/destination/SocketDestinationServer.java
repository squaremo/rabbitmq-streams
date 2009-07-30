package com.rabbitmq.streams.plugins.socket.destination;

import com.rabbitmq.streams.harness.*;
import net.sf.json.JSONObject;

import java.io.IOException;
import java.net.InetAddress;
import java.net.Socket;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CountDownLatch;

public class SocketDestinationServer extends Server {

  public final Map<String, List<Destination>> terminalMap = new ConcurrentHashMap<String, List<Destination>>();
  public final Server.ServerInputReader input = new Server.ServerInputReader() {

    @Override
    public void handleBodyForTerminal(byte[] body, String key, InputMessage ack) throws PluginException {
      List<Destination> dests = terminalMap.get(key);
      if (null != dests) {
        Thread thread = new Thread(new SendAndAcknowledge(dests, body, ack));
        thread.setDaemon(true);
        thread.start();
      }
    }
  };

  protected void terminalStatusChange(String terminalId, List<JSONObject> terminalConfigs, boolean active) {
    if (active) {
      List<Destination> dests = terminalMap.get(terminalId);
      if (null == dests || 0 == dests.size()) {
        dests = new ArrayList<Destination>(terminalConfigs.size());
        for (JSONObject termConfigObject : terminalConfigs) {
          Destination destination = new Destination(termConfigObject.getJSONObject("destination"));
          dests.add(destination);
        }
        terminalMap.put(terminalId, dests);
      }

    }
    else {
      terminalMap.remove(terminalId);
    }
  }

  @Override
  public void configure(JSONObject config) throws PluginBuildException {
    super.configure(config);
    registerInput(input);
  }


  private final class SendAndAcknowledge implements Runnable {
    private final List<Destination> destinations;
    private final byte[] body;
    private final InputMessage acknowledge;

    public SendAndAcknowledge(List<Destination> destinations, byte[] body, InputMessage acknowledge) {
      this.destinations = destinations;
      this.body = body;
      this.acknowledge = acknowledge;
    }

    public void run() {
      if (destinations.size() > 0) {
        CountDownLatch done = new CountDownLatch(destinations.size());
        ArrayList<DestinationRunner> runners = new ArrayList<DestinationRunner>();

        for (Destination destination : destinations) {
          DestinationRunner runner = new DestinationRunner(destination, body, done);
          runners.add(runner);
          Thread thread = new Thread(runner);
          thread.setDaemon(true);
          thread.start();
        }
        try {
          done.await();
        }
        catch (InterruptedException ignore) {
        }
        boolean flag = true;
        for (DestinationRunner runner : runners) {
          flag = flag && runner.success();
        }
        if (flag) {
          try {
            acknowledge.ack();
          }
          catch (MessagingException ignore) {
          }
        }

      }
    }
  }

  private final class DestinationRunner implements Runnable {
    private final Destination destination;
    private final byte[] body;
    private final CountDownLatch countDownLatch;
    private boolean success = false;

    public DestinationRunner(Destination destination, byte[] body, CountDownLatch countDownLatch) {
      this.destination = destination;
      this.body = body;
      this.countDownLatch = countDownLatch;
    }

    public void run() {
      success = destination.writeToSocket(body);
      countDownLatch.countDown();
    }

    public synchronized boolean success() {
      return success;
    }
  }

  private final class Destination {
    public Destination(JSONObject config) {
      port = config.getInt("port");
      try {
        address = InetAddress.getByName(config.getString("host"));
      }
      catch (UnknownHostException e) {
        throw new IllegalArgumentException("Unable to resolve host for inet address from configuration: " + config, e);
      }
    }

    private boolean writeToSocket(byte[] body) {
      try {
        Socket socket = new Socket(address, port);
        socket.getOutputStream().write(body);
        socket.close();
        return true;
      }
      catch (IOException e) {
        e.printStackTrace();
      }
      return false;
    }

    public InetAddress getAddress() {
      return address;
    }

    public int getPort() {
      return port;
    }

    private InetAddress address;
    private int port;

  }

}

