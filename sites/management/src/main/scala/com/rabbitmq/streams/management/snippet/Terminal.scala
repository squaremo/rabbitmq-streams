/*
 * Terminal.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package com.rabbitmq.streams.management.snippet

import net.liftweb.http.SHtml
import net.liftweb.util.Helpers._
import scala.xml.NodeSeq

import net.liftweb.widgets.tablesorter.TableSorter

import com.rabbitmq.streams.management.controller._

class Terminal {

    def table : NodeSeq = {
        <head>
            <style type="text/css">
                #terminals-list {{width: 600px;}}
            </style>
        </head>
        <div>
            {TableSorter.renderOnLoad("terminals-list")}
            <lift:comet type="Terminals">
                <t:list>Loading ...</t:list>
            </lift:comet>
        </div>
    }

}
