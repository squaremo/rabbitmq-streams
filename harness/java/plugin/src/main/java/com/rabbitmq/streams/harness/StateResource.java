/** An interface representing state storage for plugins.
 */

package com.rabbitmq.streams.harness;

import java.io.IOException;
import java.util.Map;

/**
 *
 * @author mikeb@lshift.net
 */
public interface StateResource {

  public void setState(Map<String, Object> state) throws IOException;
  public Map<String, Object> getState() throws IOException;
}
