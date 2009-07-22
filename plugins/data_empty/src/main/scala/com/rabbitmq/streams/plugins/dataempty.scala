/*
 * A plugin that moans if the data is empty.
 */

import com.rabbitmq.streams.harness.{PipelineComponent, InputReader, InputMessage,
                                     NotificationType}
import net.sf.json.JSONObject


class DataEmpty() extends PipelineComponent() {
    override def configure(config : JSONObject) {
   //    val nagMessage = config.getString("message")
   //    object input extends InputReader {
   //      override def handleMessage(msg : InputMessage) {
   //        if ( msg.body.length == 0 ) {
   //          notifier.notify(NotificationType.BadData, nagMessage)
   //        }
   //      }
   //    }
   //    registerInput("input", input)
   }
}
