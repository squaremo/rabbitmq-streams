package com.rabbitmq.streams.plugins.newdata;

import com.rabbitmq.streams.harness.*;
import java.io.UnsupportedEncodingException;
import java.util.logging.Level;
import net.sf.json.JSONObject;
import org.apache.commons.codec.binary.Base64;

import java.io.IOException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

public class NewDataComponent extends PipelineComponent {

  public NewDataComponent() {
  }

  InputReader input = new InputReader() {
    @Override
    public void handleMessage(InputMessage inputMessage) throws PluginException {
      String digest;
      try {
        digest = new String(digest(inputMessage), "ascii");
      } catch (UnsupportedEncodingException ex) {
        throw new RuntimeException("This shouldn't happen.");
      }
      try {
        if(getDatabase().getDocument(digest) == null) {
          publishToChannel("output", inputMessage);
          JSONObject document = new JSONObject();
          document.put("digest", digest);
          getDatabase().saveDocument(document, digest);
        }
      }
      catch (IOException e) {
        notifier.notify(NotificationType.FatalError, "Unable to connect to database: " + e.getMessage());
        throw new PluginException("Unable to connect to database", e);
      }
    }
  };

  byte[] digest(InputMessage message) throws PluginException {
    return digest(message.body());
  }

  byte[] digest(byte[] content) throws PluginException {
    if(null == content) {
      return new byte[0];
   }

    try {
      MessageDigest messageDigest = MessageDigest.getInstance("SHA-512");
      return Base64.encodeBase64(messageDigest.digest(content));
    }
    catch (NoSuchAlgorithmException e) {
      String msg = "Fatal system error: can't create SHA-512 digest; check your java install";
      notifier.notify(NotificationType.FatalError, msg + ": "  + e.getMessage());
      throw new PluginException(msg + ".", e);
    }
  }

  @Override
  public void configure(JSONObject jsonObject) throws PluginBuildException {
    log.debug("Configuring new data plugin");
    registerInput("input", input);
  }
}
