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
public class TransactionalInputReaderRunnable extends InputReaderRunnable {

  public TransactionalInputReaderRunnable(Object lock) {
    super();
    this.lock = lock;
  }

  @Override
  public void configure(QueueingConsumer consumer, ChannelN channel, InputHandler handler, JSONObject config, Logger log) {
    super.configure(consumer, channel, handler, config, log);
    try {
      channel.txSelect();
    } catch (IOException ioe) {
      log.fatal(ioe);
      // FIXME STOPGAP, until refactoring lands
    }
  }

  public void run() {
    while (channel.isOpen()) {
      try {
        Delivery delivery = consumer.nextDelivery();
        synchronized (lock) {
          try {
            try {
              handler.handleDelivery(delivery, mergeConfigWithHeaders(staticConfiguration, delivery.getProperties().headers));
            }
            catch (Exception e) {
              log.error("Cannot handle delivery of message");
              log.error(e);
              return;
            }
            channel.basicAck(delivery.getEnvelope().getDeliveryTag(), false);
            channel.txCommit();

          }
          catch (IOException ex) {
            try {
              log.error(ex);
              channel.txRollback();
            }
            catch (IOException e) {
              log.error(e);
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
