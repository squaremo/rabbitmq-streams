package com.rabbitmq.streams.harness;

import com.rabbitmq.client.QueueingConsumer.Delivery;

import java.io.IOException;

public interface InputReader {

    void handleDelivery(Delivery message) throws PluginException;

}
