package com.rabbitmq.streams.harness;

import net.sf.json.JSONObject;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class Run {

  public static void main(final String[] args) throws IOException, InterruptedException {
    System.out.println(args[0]);
    JSONObject config = readConfiguration();
    // FIXME Yuck, we shouldn't have to care here
    AMQPLogger buildlog = new AMQPLogger(AMQPConnection.amqConnectionFromConfig(
            config.getJSONObject("messageserver")).createChannel(),
            config.getString("plugin_name"));

    PluginBuilder builder = new PluginBuilder(buildlog);
    builder.buildPlugin(config);

    try {
      BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
      while (null != reader.readLine()) {
      }
    }
    catch (Exception e) {
      buildlog.error(e);
    }
    finally {
      buildlog.shutdown();
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