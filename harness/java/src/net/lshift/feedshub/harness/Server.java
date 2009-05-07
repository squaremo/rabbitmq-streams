package net.lshift.feedshub.harness;

/**
 * A superclass for terminal servers (ho ho).  These have predefined
 * inputs and outputs, rather than having them specified, and don't
 * enforce transactions.
 */
public class Server extends Plugin {
    
    public void ack(String deliveryTag, boolean multi) {
	this.messageChannel.basicAck(deliveryTag, multi);
    }

}
