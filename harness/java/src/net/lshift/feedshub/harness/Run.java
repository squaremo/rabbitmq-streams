package net.lshift.feedshub.harness;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.File;
import java.io.FilenameFilter;
import java.net.URL;
import java.net.URLClassLoader;

import net.sf.json.JSONObject;

public class Run {

    @SuppressWarnings("unchecked")
    public static void main(final String[] args) throws IOException,
            InterruptedException {
        System.out.println(args[0]);
        Plugin plugin = null;
        try {
            JSONObject jsonArgs = JSONObject.fromObject(new BufferedReader(
                    new InputStreamReader(System.in)).readLine());
            String pluginDir = "file://" + jsonArgs.getString("plugin_dir");
            if (!pluginDir.endsWith("/")) {
                pluginDir += "/";
            }
	    URL pluginUrl = new URL(pluginDir);
	    String[] jars = new File(pluginUrl + "lib/").list(new FilenameFilter() {
		    public boolean accept(File dir, String filename) {
			return filename.endsWith(".jar");
		    }
		});
	    URL[] classpathEntries = new URL[jars.length + 1];
	    for (int i=0; i < jars.length; i++) {
		classpathEntries[i] = new URL("file://" + jars[i]);
	    }
            String pluginName = jsonArgs.getString("plugin_name");

            ClassLoader defaultCL = ClassLoader.getSystemClassLoader();
            URLClassLoader ucl = new URLClassLoader(classpathEntries, defaultCL);
            Class<Plugin> clazz = (Class<Plugin>) ucl.loadClass(pluginName);
            plugin = clazz.getConstructor(JSONObject.class).newInstance(
                    jsonArgs);

            plugin.start();
            BufferedReader reader = new BufferedReader(new InputStreamReader(
                    System.in));
            while (null != reader.readLine()) {
            }
        } catch (Exception e) {
            e.printStackTrace();
            if (null != plugin) {
                plugin.log.error(e);
            }
        } finally {
            if (null != plugin) {
                plugin.shutdown();
            }
        }
    }
}
