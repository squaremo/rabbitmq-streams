/*
 * A plugin that evaluates a supplied JavaScript function to transform messages.
 */

package com.rabbitmq.streams.plugins.javascript

import com.rabbitmq.streams.harness.{PipelineComponent, InputReader, InputMessage}
import net.sf.json.JSONObject

import org.mozilla.javascript.Context

class JavaScriptPlugin() extends PipelineComponent() {

    private val context = Context.enter()

    private val globalScope = context.initStandardObjects(null, false)
    // TODO Add things into the scope, then seal it.
    // TODO security object

    override def configure(config : JSONObject) {
      def cleanUpFunctionValue(func : String) : String = {
        if (func.startsWith("\"") && func.endsWith("\""))
          func.substring(1, func.length-1)
        else
          func
      }

      val functionText = cleanUpFunctionValue(config.getString("function"))
      val function = context.compileFunction(globalScope, functionText, "<config>", 1, null)

      object input extends InputReader {
        override def handleMessage(msg : InputMessage) {
          val strValue = new String(msg.body)
          val context = Context.enter()
          val result = function.call(context, globalScope, function, Array(strValue))
          publishToChannel("output", msg.withBody(result.toString))
        }
      }
      registerInput("input", input)
   }
}
