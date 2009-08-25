/*
 */

package com.rabbitmq.streams.harness;

import java.nio.charset.Charset;
import java.nio.charset.CharacterCodingException;
import java.nio.ByteBuffer;
import java.util.Map;

/**
 *
 * @author mikeb@lshift.net
 * @author alexander@lshift.net
 */
public abstract class InputMessage implements Message {
  public abstract InputMessage withHeader(String key, Object val);
  public abstract InputMessage withBody(byte[] body);
  public abstract InputMessage withHeaders(Map<String, Object> headers);
  public abstract void ack() throws MessagingException;
  /** Silently replaces bad code points by '�' ('\ufffd')
   */
  public String bodyAsStringLax(){
    // This does default replacements
    return Charset.forName("utf-8").decode(ByteBuffer.wrap(this.body())).toString();
  }
  /** This will complain about bad code points
   */
  public String bodyAsString() throws CharacterCodingException {
    // This also does default replacements, and won't actually throw the
    // declared exception.  We've fixed this in branch bug21442.
    return Charset.forName("utf-8").decode(ByteBuffer.wrap(this.body())).toString();
  }
  public InputMessage withBody(String body) {
    try {
      return this.withBody(body.getBytes("utf-8"));
    }
    catch (java.io.UnsupportedEncodingException _){
        throw new RuntimeException("This shouldn't happen.");
    }
  }
  public InputMessage withContentType(String contentType) {
    return this.withHeader("Content-Type", contentType);
  }
}
