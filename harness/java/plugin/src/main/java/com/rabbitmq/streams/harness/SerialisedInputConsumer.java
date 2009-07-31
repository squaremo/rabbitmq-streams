package com.rabbitmq.streams.harness;

import com.rabbitmq.client.QueueingConsumer;
import com.rabbitmq.client.QueueingConsumer.Delivery;

import java.io.IOException;
import net.sf.json.JSONObject;

/**
 * This reader ensures that the processing of messages is serialised.
 *
 * We may have many input queues, so to avoid interleaving transactions, we have to choose either to have a channel for each, or serialise the
 * message handing. Since there are a maximum of 15 channels, we choose to serialise message handling by way of this mutex.
 * <p/>
 * Note: Transactions are only on outgoing messages, so it doesn't matter that two or more threads could receive messages before one
 * acquires the lock; the transaction will be complete or abandoned before another consumer can start sending anything.
 * 
 * QUESTION: Where does 15 come from?
 */
class SerialisedInputConsumer extends AMQPInputConsumer {

  SerialisedInputConsumer(QueueingConsumer consumer, InputHandler handler, JSONObject config, Logger log, Object lock) {
    super(consumer, handler, config, log);
    this.lock = lock;
  }

  public void run() {
    while (consumer.getChannel().isOpen()) {
      try {
        Delivery delivery = consumer.nextDelivery();
        synchronized (lock) {
          try {
            try {
              InputMessage msg = new AMQPMessage(consumer.getChannel(), delivery);
              handler.handleMessage(msg, mergeConfigWithHeaders(delivery.getProperties().headers));
              msg.ack();
            }
            catch (Exception e) {
              log.error(e);
              throw e;
            }
            consumer.getChannel().txCommit();
          }
          catch (Exception ex) {
            try {
              log.error(ex);
              consumer.getChannel().txRollback();
            }
            catch (IOException e) {
              log.fatal(e); // we cannot continue if we can't use txns
              return;
            }
          }
        }
      }
      catch (InterruptedException ignored) {
      }
    }
  }

  private final Object lock;
}
