package com.rabbitmq.streams.harness;

import com.rabbitmq.client.Channel;
import com.rabbitmq.client.AMQP;

import java.util.Map;
import java.io.IOException;

/**
 * This class abstracts the publishing of messages.
 */
public class Publisher {

  public Publisher(String exchange, Channel channel, Logger log) {
    this.exchange = exchange;
    this.channel = channel;
    this.log = log;
    basicProperties = new AMQP.BasicProperties();
    basicProperties.deliveryMode = 2;
  }

  public void publish(byte[] body) throws IOException {
    publish(body, "");
  }

  public void publish(byte[] body, String key) throws IOException {
    channel.basicPublish(exchange, key, basicProperties, body);
  }

  public void publish(byte[] body, Map<String, Object> headers) throws IOException {
    publish(body, "", headers);
  }

  public void publish(byte[] body, String key, Map<String, Object> headers) throws IOException {
    channel.basicPublish(exchange, key, propertiesWithHeaders(headers), body);
  }

  static AMQP.BasicProperties propertiesWithHeaders(Map<String, Object> headers) {
    AMQP.BasicProperties props = new AMQP.BasicProperties();
    props.deliveryMode = 2;
    props.headers = headers;
    return props;
  }

  private final String exchange;
  private final Channel channel;
  private final AMQP.BasicProperties basicProperties;
  private final Logger log;

}
