package com.rabbitmq.streams.harness;

import com.rabbitmq.client.QueueingConsumer;
import com.rabbitmq.client.QueueingConsumer.Delivery;
import net.sf.json.JSONObject;

class DefaultInputConsumer extends AMQPInputConsumer {

  DefaultInputConsumer(QueueingConsumer consumer, InputHandler handler, JSONObject config, Logger log) {
    super(consumer, handler, config, log);
  }

  public void run() {
    while (consumer.getChannel().isOpen()) {
      try {
        Delivery delivery = consumer.nextDelivery();
        InputMessage msg = new AMQPInputConsumer.AMQPMessage(consumer.getChannel(), delivery);
        handler.handleMessage(msg, mergeConfigWithHeaders(delivery.getProperties().headers));
      }
      catch (InterruptedException ignored) {
        // Carry on and try fetching again
      }
      catch (Exception ex) {
        log.error(ex);
      }
    }
  }
}
