package net.lshift.feedshub.harness;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URL;
import java.net.URLClassLoader;

import net.sf.json.JSONObject;

public class Run {

	@SuppressWarnings("unchecked")
	public static void main(final String[] args) throws IOException {
		Plugin plugin = null;
		try {
			JSONObject jsonArgs = JSONObject.fromObject(new BufferedReader(
					new InputStreamReader(System.in)).readLine());
			String pluginDir = "file://" + jsonArgs.getString("plugin_dir");
			if (!pluginDir.endsWith("/")) {
				pluginDir += "/";
			}
			String pluginName = jsonArgs.getJSONObject("config").getString(
					"type");

			ClassLoader defaultCL = ClassLoader.getSystemClassLoader();
			URLClassLoader ucl = new URLClassLoader(new URL[] { new URL(pluginDir) }, defaultCL);
			Class<Plugin> clazz = (Class<Plugin>) ucl.loadClass(pluginName);
			plugin = clazz.getConstructor(JSONObject.class).newInstance(jsonArgs);

			plugin.run();
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			if (null != plugin) {
				plugin.shutdown();
			}
		}
	}
}
