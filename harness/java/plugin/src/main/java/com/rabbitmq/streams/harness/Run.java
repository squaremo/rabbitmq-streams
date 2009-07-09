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
/*
  public static Plugin plugin(JSONObject configuration) {
    String pluginDirectory;
    String pluginName;
    Plugin plugin = null;

    try {
      configuration = JSONObject.fromObject(new BufferedReader(new InputStreamReader(System.in)).readLine());
      pluginDirectory = pluginDirectory(configuration);
      URI libUri = new URI(pluginDirectory + "lib/");

      URLClassLoader ucl = new URLClassLoader(classPathEntries(new URL(pluginDirectory), libUri, jars(libUri)), ClassLoader.getSystemClassLoader());
      Thread.currentThread().setContextClassLoader(ucl);
      pluginName = configuration.getString("plugin_name");
      @SuppressWarnings({"unchecked"}) Class<Plugin> clazz = (Class<Plugin>) ucl.loadClass(pluginName);
      plugin = clazz.getConstructor(JSONObject.class).newInstance(configuration);
    }
    catch (Exception ex) {
      System.err.println("Exception thrown while loading & constructing Java plugin");
      ex.printStackTrace(System.err);
    }
    return plugin;
  }

  private static String[] jars(URI libUri) {
    return new File(libUri).list(new FilenameFilter() {
      public boolean accept(File dir, String filename) {
        return filename.endsWith(".jar");
      }
    });
  }

  private static String pluginDirectory(JSONObject jsonArgs) {
    String pluginDir = "file://" + jsonArgs.getString("plugin_dir");
    if (!pluginDir.endsWith("/")) {
      pluginDir += "/";
    }
    return pluginDir;
  }

  private static URL[] classPathEntries(URL pluginUrl, URI libUri, String[] jars) throws MalformedURLException {
    ArrayList<URL> classpathEntries = new ArrayList<URL>();
    classpathEntries.add(pluginUrl);
    if (null != jars) {
      for (String jar : jars) {
        classpathEntries.add(new URL(libUri + jar));
      }
    }

    return classpathEntries.toArray(new URL[classpathEntries.size()]);
  }
  */

}