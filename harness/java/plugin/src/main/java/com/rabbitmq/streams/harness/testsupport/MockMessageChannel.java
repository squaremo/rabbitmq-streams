/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.rabbitmq.streams.harness.testsupport;

import com.rabbitmq.streams.harness.InputHandler;
import com.rabbitmq.streams.harness.InputMessage;
import com.rabbitmq.streams.harness.Message;
import com.rabbitmq.streams.harness.MessageChannel;
import com.rabbitmq.streams.harness.PluginException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import net.sf.json.JSONObject;

/**
 *
 * @author mikeb@lshift.net
 */
public class MockMessageChannel implements MessageChannel {

  public static class Output {
    public Message msg;
    public String channel;
    public Output(String ch, Message m) {
      msg = m;
      channel = ch;
    }
  }

  public Map<String, InputHandler> handlers = new HashMap();
  public List<Output> outputs = new ArrayList();

  public void consume(String ch, InputHandler h) {
    handlers.put(ch, h);
  }

  public void publish(String ch, Message msg) {
    outputs.add(new Output(ch, msg));
  }

  /**
   * Inject a message into a channel.
   * @param channel
   * @param m
   * @return true if the channel had a handler and completed; false if no handler.
   * @throws PluginException from the handler.
   */
  public boolean inject(String channel, InputMessage m) throws PluginException {
    InputHandler h = handlers.get(channel);
    if (h==null) return false;
    h.handleMessage(m, new JSONObject());
    return true;
  }
}
