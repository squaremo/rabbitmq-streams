package net.lshift.feedshub.harness;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.net.URL;
import java.net.URLClassLoader;

import net.sf.json.JSONObject;

public class Run {

	@SuppressWarnings("unchecked")
	public static void main(final String[] args) throws Exception {
		Plugin plugin = null;
		try {
			JSONObject jsonArgs = JSONObject.fromObject(new BufferedReader(
					new InputStreamReader(System.in)).readLine());
			String pluginDir = "file://" + jsonArgs.getString("plugin_dir");
			String pluginName = jsonArgs.getJSONObject("config").getString(
					"type");

			URLClassLoader ucl = new URLClassLoader(new URL[] { new URL(pluginDir) });
			Class<Plugin> clazz = (Class<Plugin>) ucl.loadClass(pluginName);
			plugin = clazz.getConstructor(JSONObject.class).newInstance(jsonArgs);

		} catch (Exception e) {
			throw e;
		} finally {
			if (null != plugin) {
				plugin.shutdown();
			}
		}
	}
}
