/*
 * OutputFeed.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package net.lshift.feedshub.feedserver.view

import net.liftweb.http.{LiftView, S, LiftResponse, XmlResponse, AtomResponse}
import net.liftweb.util.{Box, Full, Empty}

import net.lshift.feedshub.feedserver.model.LocalServer
import net.lshift.feedshub.feedserver.model.Archive

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
        new AtomResponse(<atom/>)
    }

    private def toRSS(archive : Archive) : LiftResponse = {
        new XmlResponse(<rss/>)
    }

}
