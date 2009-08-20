package com.rabbitmq.streams.plugins.socket.source;

import com.rabbitmq.streams.harness.MessageChannel;
import com.rabbitmq.streams.harness.PluginBuildException;
import com.rabbitmq.streams.harness.Message;
import com.rabbitmq.streams.harness.MessagingException;
import net.sf.json.JSONObject;
import org.junit.Before;
import org.junit.Test;
import static org.junit.Assert.*;
import static org.mockito.Mockito.*;
import org.mockito.ArgumentMatcher;

import java.util.ArrayList;
import java.util.List;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.BufferedWriter;
import java.net.UnknownHostException;
import java.net.Socket;
import java.net.InetAddress;

public class SocketSourceServerTest {

  private SocketSourceServer server;
  private MessageChannel channel;
  private static final String SAUSAGES = "SAUSAGES";

  @Before
  public void setup() throws PluginBuildException {
    channel = mock(MessageChannel.class);

    server = new SocketSourceServer();
    server.setMessageChannel(channel);

    server.configure(null);

  }

  @Test
  public void test()  {
    assertTrue(true);
  }

//  @Test
  public void testReceive() throws MessagingException {
    JSONObject terminal = new JSONObject();
    terminal.put("source", JSONObject.fromObject("{\"port\":20468}"));
    List<JSONObject> terminals = new ArrayList<JSONObject>();
    terminals.add(terminal);

    server.terminalStatusChange("socket_source_server", terminals, true);
    writeToSocket(SAUSAGES);
    @SuppressWarnings({"unchecked"}) ArgumentMatcher<Message> matcher = new IsMessageThatMatchesContent(SAUSAGES);
    verify(channel).publish(eq("output"), argThat(matcher));
  }

  private class IsMessageThatMatchesContent extends ArgumentMatcher  {
    private String match;

    public IsMessageThatMatchesContent(String match) {
      this.match = match;
    }

    public boolean matches(Object o) {
      if(o instanceof Message)  {
        Message message = (Message) o;
        //FIXME(alexander): why isn't this a byte-array comparison
        try {
            return match.equals(new String(message.body(), "utf-8"));
        }
        catch (Exception _) {throw new RuntimeException("Kaboom!");}
      }
      return false;
    }
  }

  private void writeToSocket(String content) {
    BufferedWriter wr = null;
    Socket socket = null;
    try {
      InetAddress addr = InetAddress.getByName("localhost");
      int port = 20468;

      socket = new Socket(addr, port);

      wr = new BufferedWriter(new OutputStreamWriter(socket.getOutputStream()));
      wr.write(content);
      wr.flush();

    }
    catch (UnknownHostException e) {
      e.printStackTrace();
    }
    catch (IOException e) {
      e.printStackTrace();
    }
    finally {
      try {
        if (wr != null) {
          wr.close();
        }
        if (socket != null) {
          socket.close();
        }
      }
      catch (IOException e) {
        e.printStackTrace();
      }
    }

  }
}
