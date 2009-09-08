package com.rabbitmq.streams.harness;

/**
 *
 * @author mikeb
 */
public class MessagingException extends PluginException {

  MessagingException(String msg) {
    super(msg);
  }

  MessagingException(String string, Exception ex) {
    super(string, ex);
  }

}
