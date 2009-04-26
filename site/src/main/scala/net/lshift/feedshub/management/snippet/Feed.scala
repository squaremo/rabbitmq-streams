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

    def add : NodeSeq = {
        Console.println("Add form render")
        SHtml.submit("Add a feed", () => Feeds ! AddFeed(new FeedDefinition))
    }
}
