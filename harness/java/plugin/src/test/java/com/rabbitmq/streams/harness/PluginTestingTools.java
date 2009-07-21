/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.rabbitmq.streams.harness;

import net.sf.json.JSONArray;
import net.sf.json.JSONObject;

/**
 *
 * @author mikeb
 */
public class PluginTestingTools {

  public static JSONObject minimalConfig() {
    JSONObject res = new JSONObject();
    JSONObject ptype = new JSONObject();
    ptype.put("global_configuration_specification", new JSONArray());
    res.put("plugin_type", ptype);
    res.put("configuration", new JSONObject());
    return res;
  }

}
