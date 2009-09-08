package com.rabbitmq.streams.management.snippet


import net.liftweb.http.SHtml
import net.liftweb.util.Helpers._
import scala.xml.NodeSeq

import net.liftweb.widgets.tablesorter.TableSorter

import com.rabbitmq.streams.management.controller._

class Feed {

    def add : NodeSeq = {
        SHtml.submit("Add a feed", () => Feeds ! AddFeed(new FeedDefinition))
    }

    def table : NodeSeq = {
        <head>
            <style type="text/css">
                #feeds-list {{width: 600px;}}
            </style>
        </head>
        <div>
            {TableSorter.renderOnLoad("feeds-list")}
            <lift:comet type="FeedsActor">
                <f:list>Loading ...</f:list>
            </lift:comet>
        </div>
    }
}
