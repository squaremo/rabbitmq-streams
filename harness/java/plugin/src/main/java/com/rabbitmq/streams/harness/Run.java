package com.rabbitmq.streams.harness;

import com.rabbitmq.client.Connection;
import net.sf.json.JSONObject;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.BitSet;

public class Run {
  static final int DEBUG = 1;
  static final int TRACE = 2;

  private JSONObject config;
  private Connection connection;
  private AMQPLogger buildlog;

  public Run()  {
    Thread thread = new Thread()  {
      @Override
      public void run() {
        if(connection != null)  {
          try {
            if(connection.isOpen()) {
              connection.close();
            }
          }
          catch (IOException e) {
            System.out.println("IO Exception thrown when shutting down plugin" + e);
          }
        }
      }
    };
    thread.setDaemon(true);
    Runtime.getRuntime().addShutdownHook(thread);
  }
  
  public void setConfig(JSONObject config) {
    this.config = config;
  }

  public void runPlugin() throws IOException  {
    connection = new AMQPConnectionFactory().connectionFromConfig(config.getJSONObject("messageserver"));
    try {
      BitSet flags = new BitSet();
      if (config.optBoolean("debug", false)) flags.set(DEBUG);
      if (config.optBoolean("trace", false)) flags.set(TRACE);

      buildlog = new AMQPLogger(connection.createChannel(), "." + config.getString("plugin_name"), flags);
      // TODO: why does this need to run in a thread, rather than just being synchronous
      // TODO: encapsulate this
      Thread logThread = new Thread(buildlog);
      logThread.setDaemon(true);
      logThread.start();

      SessionFactory sf = new SessionFactory();
      PluginResourceFactory factory = new PluginResourceFactory(connection, sf, buildlog);
      factory.setFlags(flags);
      PluginBuilder builder = new PluginBuilder(buildlog, factory);
      builder.setFlags(flags);
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
      connection.close();
    }
  }

  public static void main(final String[] args) throws IOException, InterruptedException {
    System.out.println(args[0]);
    Run run = new Run();
    run.setConfig(readConfiguration());
    run.runPlugin();
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