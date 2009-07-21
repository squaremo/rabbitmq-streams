package com.rabbitmq.streams.harness;

import com.rabbitmq.client.Channel;
import com.rabbitmq.client.AMQP;

import java.util.Map;
import java.io.IOException;

/**
 * This class abstracts the publishing of messages.
 */
public class AMQPPublisher {

  public AMQPPublisher(String exchange, Channel channel) {
    this.exchange = exchange;
    this.channel = channel;
    basicProperties = new AMQP.BasicProperties();
    basicProperties.deliveryMode = 2;
  }

  public void publish(Message msg) throws IOException {
    channel.basicPublish(exchange, msg.routingKey(), propertiesWithHeaders(msg.headers()), msg.body());
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

}
