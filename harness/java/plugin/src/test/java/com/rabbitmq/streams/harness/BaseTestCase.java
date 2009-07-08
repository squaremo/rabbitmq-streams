package com.rabbitmq.streams.harness;

import junit.framework.TestCase;
import net.sf.json.JSONObject;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;

abstract public class BaseTestCase extends TestCase {
  
  public JSONObject getDefaultConfiguration() {
    BufferedReader reader = new BufferedReader(new InputStreamReader(getClass().getResourceAsStream("/test-config.js")));
    StringBuffer buffer = new StringBuffer();
    String str;
    try {
      while((str = reader.readLine()) != null)  {
        buffer.append(str);
      }
    }
    catch (IOException e) {
      e.printStackTrace();
    }
    return JSONObject.fromObject(buffer.toString());
  }


}
