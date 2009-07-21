package com.rabbitmq.streams.harness;

import com.rabbitmq.client.QueueingConsumer;
import com.rabbitmq.client.QueueingConsumer.Delivery;

import com.rabbitmq.client.impl.ChannelN;
import java.io.IOException;
import net.sf.json.JSONObject;

/**
 * This reader ensures that the reading of messages is synchronised.
 *
 * We may have many input queues, so to avoid interleaving transactions, we have to choose either to have a channel for each, or serialise the
 * message handing. Since there are a maximum of 15 channels, we choose to serialise message handling by way of this mutex.
 * <p/>
 * Note: Transactions are only on outgoing messages, so it doesn't matter that two or more threads could receive messages before one
 * acquires the lock; the transaction will be complete or abandoned before another consumer can start sending anything.
 */
class TransactionalInputReaderRunnable extends AMQPInputConsumer {

  TransactionalInputReaderRunnable(QueueingConsumer consumer, InputHandler handler, JSONObject config, Object lock) {
    super(consumer, handler, config);
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
              // FIXME report this!
              return;
            }
            consumer.getChannel().txCommit();
          }
          catch (IOException ex) {
            try {
              // FIXME log.error(ex);
              consumer.getChannel().txRollback();
            }
            catch (IOException e) {
              // FIXME log.error(e);
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
