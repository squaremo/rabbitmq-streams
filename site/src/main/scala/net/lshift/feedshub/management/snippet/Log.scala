/*
 * Log.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.lshift.feedshub.management.snippet

import net.liftweb.http.SHtml
import net.liftweb.util.Helpers._
import scala.xml.NodeSeq

class Log {

    def list: NodeSeq = {
        <head>
            <style type="text/css">
                #log-list {{scrolling: auto; height: 200px; width: 600px;}}
                #log-list .control-button {{}}
                #log-list dt {{width: 100%;}}
                dt.info {{background: #eee;}}
                dt.debug {{color: #ccc}}
                dt.fatal, dt.error {{background: red, color: white;}}
                dt.warn {{background: blue;color: white;}}
            </style>
        </head>
        <lift:comet type="LogActor">
            <div id="log-list">
                <log:controls/>
                <log:list>
                    Loading ..
                </log:list>
            </div>
        </lift:comet>
    }
}
