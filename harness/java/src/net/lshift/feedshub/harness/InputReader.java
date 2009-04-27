package net.lshift.feedshub.harness;

import java.io.IOException;

import com.rabbitmq.client.Consumer;
import com.rabbitmq.client.Envelope;
import com.rabbitmq.client.ShutdownSignalException;
import com.rabbitmq.client.AMQP.BasicProperties;

public abstract class InputReader implements Consumer {

	public void handleCancelOk(String consumerTag) {
		// ignore this
	}

	public void handleConsumeOk(String consumerTag) {
		// ignore this
	}

	public abstract void handleDelivery(String Consumer, Envelope envelope,
			BasicProperties properties, byte[] msg) throws IOException;

	public void handleShutdownSignal(String consumerTag,
			ShutdownSignalException sig) {
		// ignore this
	}

}
