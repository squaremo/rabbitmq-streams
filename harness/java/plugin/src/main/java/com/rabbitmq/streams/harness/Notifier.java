package com.rabbitmq.streams.harness;

import com.rabbitmq.client.AMQP.BasicProperties;
import com.rabbitmq.client.ShutdownSignalException;
import com.rabbitmq.client.impl.ChannelN;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.util.Collections;
import java.util.Map;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.SynchronousQueue;
import java.util.logging.Level;
import java.util.logging.Logger;

public class Notifier implements Runnable {
  private static final Map<String, Object> noheaders = Collections.emptyMap();

  private static final String exchange = "feedshub/notify";
  private final NotificationMessage shutdownMessage = new NotificationMessage(NotificationType.StopNotifier, null, noheaders);

  private final ChannelN channel;
  private final String routingKey;

  private final BlockingQueue<NotificationMessage> queue = new SynchronousQueue<NotificationMessage>();

  public Notifier(ChannelN channel, String routingKey) {
    this.channel = channel;
    this.routingKey = routingKey;
  }

  public void notify(NotificationType type, String message) {
    notify(type, message, noheaders);
  }

  public void notify(NotificationType type, String message, Map<String, Object> headers) {
    try {
      queue.put(new NotificationMessage(type, message, headers));
    }
    catch (InterruptedException e) {
      notify(type, message, headers);
    }
  }

  public void run() {
    NotificationMessage message;
    while (true) {
      message = null;
      while (null == message) {
        try {
          message = queue.take();
        }
        catch (InterruptedException ignored) {
        }
      }
      if (message == shutdownMessage) {
        break;
      }
      else {
        try {
          channel.basicPublish(exchange, message.typeLabel() + routingKey, false, false, message.basicProperties(), message.contents());
        }
        catch (IOException e) {
          e.printStackTrace();
        }
      }
    }
    if (channel.isOpen()) {
      try {
        channel.close();
      }
      catch (ShutdownSignalException ignored) {
      }
      catch (IOException ignored) {
      }
    }
  }


  private class NotificationMessage {
    private final NotificationType type;
    private final String message;
    private final Map<String, Object> headers;

    public NotificationMessage(NotificationType type, String message, Map<String, Object> headers) {
      this.type = type;
      this.message = message;
      this.headers = headers;
    }

    public byte[] contents() {
      try {
        return message.getBytes("utf-8");
      } catch (UnsupportedEncodingException ex) {
        throw new RuntimeException("This shouldn't happen.");
      }
    }

    public String typeLabel() {
      return type.getLabel();
    }

    public BasicProperties basicProperties() {
      BasicProperties properties =  new BasicProperties();
      properties.headers = headers;
      properties.deliveryMode = 2;

      return properties;
    }
  }

}




