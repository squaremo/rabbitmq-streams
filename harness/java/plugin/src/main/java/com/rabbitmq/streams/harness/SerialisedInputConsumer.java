package com.rabbitmq.streams.harness;

import com.rabbitmq.client.QueueingConsumer;
import com.rabbitmq.client.QueueingConsumer.Delivery;

import java.io.IOException;
import net.sf.json.JSONObject;

/**
 * This input consumer ensures that the processing of messages is serialised,
 * and handles transactions and acking for the receiver.
 *
 * We may have many input queues, so to avoid interleaving transactions, we have to choose either to have a channel for each, or serialise the
 * message handing. Since there are a maximum of 15 channels, we choose to serialise message handling by way of this mutex.
 * <p/>
 * Note: Transactions are only on outgoing messages, so it doesn't matter that two or more threads could receive messages before one
 * acquires the lock; the transaction will be complete or abandoned before another consumer can start sending anything.
 * 
 * QUESTION: Where does 15 come from?
 *
 * @author matthew@lshift.net
 * @author mikeb@lshift.net
 */
class SerialisedInputConsumer extends AMQPInputConsumer {

  /**
   * @param consumer
   * @param handler
   * @param config
   * @param log
   * @param lock The object used as a mutex between consumers; give the same
   * lock to all the input consumers that should be mutually serialised.
   */
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
              // is this block needed? the exception will be caught outside,
              // and before the transaction commits.
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
