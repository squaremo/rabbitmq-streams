package com.rabbitmq.streams.harness;

import com.rabbitmq.client.QueueingConsumer.Delivery;

public interface InputReader {

    void handleDelivery(Delivery message) throws Exception;

}
