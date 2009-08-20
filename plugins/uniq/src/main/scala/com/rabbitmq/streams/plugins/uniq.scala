/*
 * A plugin that moans if there are input duplicates.
 */

package com.rabbitmq.streams.plugins

import java.util.Arrays

import com.rabbitmq.streams.harness.{PipelineComponent, InputReader, InputMessage,
                                     NotificationType}
import net.sf.json.{JSONObject, JSONArray}




class UniqPlugin() extends PipelineComponent() {
  //FIXME(alexander): AARGH. Surely there *must* be a saner way to do this!?
  //and why does that have to go into the class?
    def unpickleBody(body: Object): Array[Byte] = {
      JSONArray.toArray(JSONArray.fromObject(body)).asInstanceOf[Array[Object]].map(
        _.asInstanceOf[Int].toByte)
    }
    override def configure(config : JSONObject) {
      var state = getState()
      var lastMessage:Array[Byte] =
        if (state containsKey "lastMessage") {
          unpickleBody(state.get("lastMessage"))
        }
        else {
          null }

      object input extends InputReader {
        override def handleMessage(msg : InputMessage) {
          // FIXME(alexander): should (robustly) hash for storage efficiency
          if (! Arrays.equals(msg.body, lastMessage)) {
            publishToChannel("output", msg)
            lastMessage = msg.body
            state.put("lastMessage", JSONArray.fromObject(lastMessage))
            setState(state)
          }
        }
      }
      registerInput("input", input)
   }
}
