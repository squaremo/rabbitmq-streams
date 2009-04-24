/*
 * Feed.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.lshift.feedshub.management.snippet


import net.liftweb.http.SHtml
import net.liftweb.util.Helpers._
import scala.xml.NodeSeq

import net.lshift.feedshub.management.controller._

class Feed {
    def list : NodeSeq = {
        <lift:comet type="FeedsActor">
            <lift:view>Loading list ..</lift:view>
        </lift:comet>
    }

    def add : NodeSeq = {
        SHtml.submit("Add a feed", ignore => Feeds ! AddFeed(new FeedDefinition))
    }
}
