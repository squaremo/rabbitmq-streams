/*
*/

package com.rabbitmq.streams.harness;

import java.util.Map;

/**
 *
 * @author mikeb@lshift.net
 */
public interface Logger {

  void debug(Throwable t);

  void debug(Throwable t, String label);

  void debug(String message);

  void debug(String message, String label);

  void debug(String message, Map<String, Object> headers);

  void error(Throwable t);

  void error(String message);

  void error(String message, String label);

  void error(String message, Map<String, Object> headers);

  void fatal(Throwable t);

  void fatal(String message);

  void fatal(String message, String label);

  void fatal(String message, Map<String, Object> headers);

  void info(Throwable t);

  void info(String message);

  void info(String message, String label);

  void info(String message, Map<String, Object> headers);

  void warn(Throwable t);

  void warn(String message);

  void warn(String message, String label);

  void warn(String message, Map<String, Object> headers);

}
