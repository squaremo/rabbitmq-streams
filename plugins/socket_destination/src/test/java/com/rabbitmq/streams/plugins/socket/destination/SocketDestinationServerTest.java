package com.rabbitmq.streams.plugins.socket.destination;

import com.rabbitmq.streams.harness.InputMessage;
import com.rabbitmq.streams.harness.PluginException;
import net.sf.json.JSONObject;
import org.junit.Before;
import org.junit.Test;
import static org.mockito.Mockito.mock;

import java.io.*;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.ArrayList;
import java.util.List;

public class SocketDestinationServerTest {
  private SocketDestinationServer server;
  private InputMessage message;
  private int port = 24067;

  @Before
  public void setup() {
    server = new SocketDestinationServer();
    message = mock(InputMessage.class);

    SocketListener socketListener = new SocketListener();
    Thread listener = new Thread(socketListener);
    listener.setDaemon(true);
    listener.start();
  }

  @Test
  public void testHandleMessage() {
    JSONObject terminal = new JSONObject();
    terminal.put("destination", JSONObject.fromObject("{\"port\":24067, \"host\":\"localhost\"}"));
    List<JSONObject> terminals = new ArrayList<JSONObject>();
    terminals.add(terminal);
    
    try {
      server.terminalStatusChange("destination", terminals, true);
      server.input.handleBodyForTerminal("sausages".getBytes(), "destination", message);
    }
    catch (PluginException e) {
      e.printStackTrace();
    }
  }

  private class SocketListener implements Runnable {

    public void run() {
      Socket socket = null;
      try {
        ServerSocket serverSocket = new ServerSocket(port);
        socket = serverSocket.accept();
        BufferedReader in = new BufferedReader(new InputStreamReader(socket.getInputStream()));
        String input;
        while ((input = in.readLine()) != null) {
          System.out.println("INPUT IS " + input);
        }
        System.out.println("DONE READING FROM SOCKET");
      }
      catch (IOException e) {
        e.printStackTrace();
      }
      finally {
        if (socket != null) {
          try {
            socket.close();
          }
          catch (IOException e) {
            e.printStackTrace();
          }
        }
      }
    }
  }
}
