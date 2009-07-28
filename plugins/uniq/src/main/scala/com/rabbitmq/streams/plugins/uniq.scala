/*
 * A plugin that moans if there are input duplicates.
 */

package com.rabbitmq.streams.plugins

import com.rabbitmq.streams.harness.{PipelineComponent, InputReader, InputMessage,
                                     NotificationType}
import net.sf.json.JSONObject

//FIXME(alexander): looks like arrays in scala have broken "equals", in
// any event this doesn't work w/o going thru the indirection of creating
// strings.
class UniqPlugin() extends PipelineComponent() {
    override def configure(config : JSONObject) {
      var state = getState()
      var lastMessage:String =
        if (state containsKey "lastMessage") {
          state.get("lastMessage").asInstanceOf[String] }
        else {
          null }
      object input extends InputReader {
        override def handleMessage(msg : InputMessage) {
          val thisMessage = new String(msg.body)
          // FIXME(alexander): should (robustly) hash for storage efficiency
          if (thisMessage != lastMessage) {
            publishToChannel("output", msg)
            lastMessage = thisMessage
            state.put("lastMessage", lastMessage)
            setState(state)
          }
        }
      }
      registerInput("input", input)
   }
}
