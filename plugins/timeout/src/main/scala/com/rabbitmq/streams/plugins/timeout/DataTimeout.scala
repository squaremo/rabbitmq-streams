package com.rabbitmq.streams.plugins.timeout

import net.sf.json.JSONObject
import com.rabbitmq.streams.harness.PipelineComponent
import com.rabbitmq.streams.harness.InputReader

/**
 * A plugin that sends a notification when it hasn't seen data for the configured
 * time.  Compose with deduplication to alert when there's no different data; compose with
 * dispatch to alert when there's no matching data.
 *
 */
class DataTimeout extends PipelineComponent {

  override def configure(config : JSONObject) {
    object input extends InputReader {
      
    }
    registerInput("input", input)
  }

}
