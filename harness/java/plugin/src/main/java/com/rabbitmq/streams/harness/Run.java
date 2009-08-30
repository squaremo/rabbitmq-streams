package com.rabbitmq.streams.harness;

import com.rabbitmq.client.Connection;
import net.sf.json.JSONObject;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class Run {

  private JSONObject config;
  private Connection connection;
  private AMQPLogger buildlog;

  public Run()  {
    Runtime.getRuntime().addShutdownHook(new Thread(){
        public void run() {
          if(buildlog != null)  {
            buildlog.shutdown();
          }
          if(connection != null)  {
            try {
              connection.close();
          }
            catch (IOException e) {
              e.printStackTrace();
            }
          }
        }
      });
  }
  
  public void setConfig(JSONObject config) {
    this.config = config;
  }

  public void runPlugin() throws IOException  {
    connection = new AMQPConnectionFactory().connectionFromConfig(config.getJSONObject("messageserver"));
    try {
      AMQPLogger buildlog = new AMQPLogger(connection.createChannel(), "." + config.getString("plugin_name"));
      // TODO: why does this need to run in a thread, rather than just being synchronous
      // TODO: encapsulate this
      Thread logThread = new Thread(buildlog);
      logThread.setDaemon(true);
      logThread.start();

      SessionFactory sf = new SessionFactory();
      PluginResourceFactory factory = new PluginResourceFactory(connection, sf, buildlog);
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