/*
 * OutputFeed.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package com.rabbitmq.streams.feedserver.view

import net.liftweb.http.{LiftView, S, LiftResponse, XmlResponse, AtomResponse}
import net.liftweb.util.{Box, Full, Empty}

import com.rabbitmq.streams.feedserver.model.LocalServer
import com.rabbitmq.streams.feedserver.model.Archive

object OutputFeed {

    private def feed(toWhatever : Archive => LiftResponse)(name : String) : Box[LiftResponse] = {
        LocalServer.archive(name) match {
            case Some(archive) =>
                Full(toWhatever(archive))
            case None => Empty
        }
    }

    def rss(name : String) : Box[LiftResponse] = {
        feed(toRSS)(name)
    }
    
    def atom(name : String) : Box[LiftResponse] = {
        feed(toAtom)(name)
    }

    private def toAtom(archive : Archive) : LiftResponse = {
        new AtomResponse(
            <atom:feed xmlns="http://www.w3.org/2005/Atom">
                <atom:title type="text">{archive.title}</atom:title>
                <atom:entries>
                    {for (entry <- archive.entries(10))
                     yield (<atom:entry>
                                <atom:updated>{entry.updated}</atom:updated>
                                <atom:content type="text/html">
                                    {entry.content}
                                </atom:content>
                            </atom:entry>)}
                </atom:entries>
            </atom:feed>)
    }

    private def toRSS(archive : Archive) : LiftResponse = {
        new XmlResponse(<rss/>)
    }

}
