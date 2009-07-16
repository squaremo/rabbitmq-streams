/**
 * An exception for the messaging system to signal problems.
 */

package com.rabbitmq.streams.harness;

import java.io.IOException;

/**
 *
 * @author mikeb
 */
public class MessagingException extends Exception {

  MessagingException(String msg) {
    super(msg);
  }

  MessagingException(String string, Exception ex) {
    super(string, ex);
  }

}
