package com.rabbitmq.streams.harness;

import com.rabbitmq.client.QueueingConsumer.Delivery;

public class DefaultInputReaderRunnable extends InputReaderRunnable {
  public void run() {
    while (channel.isOpen()) {
      try {
        Delivery delivery = consumer.nextDelivery();
        try {
          handler.handleDelivery(delivery, mergeConfigWithHeaders(staticConfiguration, delivery.getProperties().headers));
        }
        catch (Exception e) {
          log.error("Cannot handle delivery of message");
          log.error(e);
        }
      }
      catch (InterruptedException ignored) {
        // Carry on and try fetching again
      }
    }
  }
}