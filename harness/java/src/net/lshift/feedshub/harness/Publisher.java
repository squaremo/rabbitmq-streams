package net.lshift.feedshub.harness;

import java.io.IOException;

public interface Publisher {
	
	void publish(byte[] body) throws IOException;

}
