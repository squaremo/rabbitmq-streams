package com.rabbitmq.streams.harness;

import com.rabbitmq.client.Connection;
import net.sf.json.JSONObject;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class Run {

  private JSONObject config;

  public void setConfig(JSONObject config) {
    this.config = config;
  }

  public void runPlugin() throws IOException  {
    Connection conn = AMQPConnection.amqConnectionFromConfig(config.getJSONObject("messageserver"));
    try {
      AMQPLogger buildlog = new AMQPLogger(conn.createChannel(), "." + config.getString("plugin_name"));
      // TODO: why does this need to run in a thread, rather than just being synchronous
      // TODO: encapsulate this
      Thread logThread = new Thread(buildlog);
      logThread.setDaemon(true);
      logThread.start();

      SessionFactory sf = new SessionFactory();
      PluginResourceFactory factory = new PluginResourceFactory(conn, sf, buildlog);
      PluginBuilder builder = new PluginBuilder(buildlog, factory);
      builder.buildPlugin(config);

      try {
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        while (null != reader.readLine()) {
        }
      }
      catch (IOException e) {
        buildlog.error(e);
      }
      finally {
        buildlog.shutdown();
      }
    }
    finally {
      conn.close();
    }
  }

  public static void main
    (
      final String[] args) throws IOException, InterruptedException {
    System.out.println(args[0]);
    Run run = new Run();
    run.setConfig(readConfiguration());
    run.runPlugin();
  }

  private static JSONObject readConfiguration
    () {
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