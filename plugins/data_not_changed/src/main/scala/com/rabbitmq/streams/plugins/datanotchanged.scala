/*
 * A plugin that moans if there are input duplicates.
 */

package com.rabbitmq.streams.plugins

import com.rabbitmq.streams.harness.{PipelineComponent, InputReader, InputMessage,
                                     NotificationType}
import net.sf.json.JSONObject

class DataNotChangedPlugin() extends PipelineComponent() {
    override def configure(config : JSONObject) {
      var state = getState()
      var lastMessage:String =
        if (state containsKey "lastMessage") {
          state.get("lastMessage").asInstanceOf[String] }
        else {
          null }
      val nagMessage = config.getString("message")

      object input extends InputReader {
        override def handleMessage(msg : InputMessage) {
          val thisMessage = new String(msg.body)
          if (thisMessage == lastMessage) {
            DataNotChangedPlugin.this.notification(NotificationType.BadData, nagMessage)
            lastMessage = thisMessage
            state.put("lastMessage", lastMessage)
            setState(state)
          }
        }
      }
      registerInput("input", input)
   }
}
