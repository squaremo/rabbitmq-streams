/*
 * archive.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

import net.lshift.feedshub.harness.Plugin
import net.sf.json._
import net.lshift.feedshub.plugin.archive._

class archive(config : JSONObject) extends Plugin(config) {

    private val terminalsUrl = config.getString("terminals_url")

    private val dispatcher = new Dispatcher(terminals_url)

    def configure = {

    }
}
