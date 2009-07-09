package com.rabbitmq.streams.harness;

import net.sf.json.JSONObject;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class Run {

  public static void main(final String[] args) throws IOException, InterruptedException {
    System.out.println(args[0]);
    Harness harness = new Harness(readConfiguration());

    try {
      harness.start();
      BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
      while (null != reader.readLine()) {
      }
    }
    catch (Exception e) {
      e.printStackTrace();
    }
    finally {
      harness.shutdown();
    }

  }

  private static JSONObject readConfiguration() {
    try {
      return JSONObject.fromObject(new BufferedReader(new InputStreamReader(System.in)).readLine());
    }
    catch (IOException e) {
      System.err.println("Unable to read configuration from standard input");
      e.printStackTrace();
    }
    return null;
  }

}