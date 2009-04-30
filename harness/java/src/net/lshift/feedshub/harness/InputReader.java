package net.lshift.feedshub.harness;

import com.rabbitmq.client.QueueingConsumer.Delivery;

public interface InputReader {

    void handleDelivery(Delivery message) throws Exception;

}
