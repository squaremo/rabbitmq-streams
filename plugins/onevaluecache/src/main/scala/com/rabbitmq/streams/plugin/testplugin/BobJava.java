package com.rabbitmq.streams.plugin.testplugin;

import net.sf.json.JSONObject;

public class BobJava {
    public static JSONObject jim()
    {
        JSONObject ret = JSONObject.fromObject("{            \"name\": \"One Value Cache Attacher\",            \"author\": {\"name\": \"James Kearney\", \"email\": \"james@lshift.net\"},            \"type\": \"plugin-specification\",            \"harness\": \"java\",            \"subtype\": \"pipeline_component\",                        \"global_configuration_specification\": [],            \"configuration_specification\": [],            \"inputs_specification\": [{\"name\": \"cache\", \"label\": \"Cache\"},{\"name\": \"input\", \"label\": \"Input\"}],            \"outputs_specification\": [{\"name\": \"output\", \"label\": \"Output\"}]}");
        
        return ret;
    }
}
