package com.rabbitmq.streams.plugin.testplugin

import java.io.IOException;
import java.util.HashMap;
import net.sf.json.JSONObject;

import com.rabbitmq.streams.harness.InputReader;
import com.rabbitmq.streams.harness.PipelineComponent;
import com.rabbitmq.streams.harness.PipelineComponent.PipelinePublisher;
import com.rabbitmq.streams.harness.PluginException;

import scala.collection.mutable.Map
import scala.collection.jcl.Conversions._

class bob(config: JSONObject) extends PipelineComponent(config) {
    private var cachedValue: String = null;
    
    val cache = new InputReader() {
      override def handleBody(body: Array[Byte])
      {
        cachedValue = new String(body);
      }
    }
    
    val input = new InputReader() {
      override def handleBody(body: Array[Byte])
      {
            val headers = new HashMap[java.lang.String, java.lang.Object]();
            
            if (cachedValue != null)
            {
                //Check message against cachedValue
                headers += ("cacheSCALA" -> cachedValue);
            }
            
            //Send on message possibly modified
            try {
                output.publish(body, headers);
            } catch {
              case e: IOException =>
                throw new PluginException(e);
            }
      }
    }
    
    var output: PipelinePublisher = null;
    
    postConstructorInit();
}