/*
 * A plugin that moans if the data is empty.
 */

package com.rabbitmq.streams.plugins.dataempty

import com.rabbitmq.streams.harness.{PipelineComponent, InputReader, InputMessage,
                                     NotificationType}
import net.sf.json.JSONObject

class DataEmptyPlugin() extends PipelineComponent() {
  override def configure(config : JSONObject) {
    val nagMessage = config.getString("message")
    object input extends InputReader {
      override def handleMessage(msg : InputMessage) {
        if ( msg.body() == null || msg.body().length == 0 ) {
          DataEmptyPlugin.this.notification(NotificationType.BadData, nagMessage)
        }
      }
    }
    registerInput("input", input)
  }
}
