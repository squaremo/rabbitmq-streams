/*
 * Subscription.scala
 */

package com.rabbitmq.streams.plugin.websubscriber

import scala.actors.Actor
import scala.actors.Actor._

import net.sf.json._

import com.rabbitmq.streams.harness.Logger

import net.liftweb.util.ActorPing
import net.liftweb.util.Helpers._
import java.util.concurrent.ScheduledFuture
import java.net.{URL, URLConnection, HttpURLConnection}
import java.util.Date
import java.io.InputStream

import scala.collection.jcl.Conversions._

import com.sun.syndication.feed.synd.{SyndFeed, SyndEntry}
import com.sun.syndication.io.{SyndFeedInput, SyndFeedOutput}
import com.sun.syndication.io.XmlReader
import com.sun.syndication.io.impl.Atom10Generator

import org.jdom.output.Format
import org.jdom.output.XMLOutputter
import org.jdom.filter.ElementFilter
import org.jdom.Element

case object RetrieveNow
case class StopPolling(reason : String)
case class Retrieved(content: Response, state : State)

case class Response(result: PollResult.PollResult, feed: Option[SyndFeed])

// We use these to decide what to do next.
// See http://diveintomark.org/archives/2003/07/21/atom_aggregator_behavior_http_level
// for the ideal.
object PollResult extends Enumeration("ok", "notfound", "notmodified", "gone", "accessdenied", "error", "unknown") {
    type PollResult = Value
    val OK, NotFound, NotModified, Gone, Forbidden, Error, Unknown = Value
}

case class State(currentUrl : String, originalUrl : String,
                 lastUpdated : Long,
                 interval : Int,
                 lastResult: PollResult.PollResult,
                 etag: Option[String],
                 lastModified: Option[Long]) {

    def updatedAt(updated : Long, result : PollResult.PollResult) : State = {
        new State(currentUrl, originalUrl, updated, interval, result, etag, lastModified)
    }

    def withNewURL(url : String) : State = {
        new State(url, originalUrl, lastUpdated, interval, lastResult, etag, lastModified)
    }

    def withETag(etag : String) : State = {
        new State(currentUrl, originalUrl, lastUpdated, interval, lastResult, Some(etag), lastModified)
    }

    def withLastModified(lastModified: Long) : State = {
        new State(currentUrl, originalUrl, lastUpdated, interval, lastResult, etag, Some(lastModified))
    }
}

object Subscription {
    val ContentBearingResponseCodes = Set(200, 301, 302, 303, 307)
    val StopResults = Set(PollResult.Forbidden, PollResult.Gone)
}
class Subscription(log : Logger, initialState: State, saveState: State => Unit, publish: String => Unit) extends Actor {

    def loop(alarm: ScheduledFuture[AnyRef],  state: State) {
        react {
            case RetrieveNow => poll(state); loop(setAlarm(state.interval), state)
                // don't try to interrupt something that's already running,
                // just to avoid complication
            case StopPolling(reason) => stopPolling("Told to stop: " + reason, alarm)
            case Retrieved(content, responseState) => {
                    if (shouldContinue(responseState.lastResult)) {
                        maybePublish(content, responseState)
                        // TODO back off for some results
                        loop(alarm, responseState)
                    }
                    else stopPolling(responseState.lastResult, alarm)
                }
        }
    }

    private def shouldContinue(result : PollResult.PollResult) : Boolean = ! Subscription.StopResults.contains(result)

    private def stopPolling(reason: AnyRef, alarm : ScheduledFuture[AnyRef]) {
        log.info("Stopping: " + reason.toString)
        alarm.cancel(false)
    }

    private def extractConditionalHeaders(state : State, conn : HttpURLConnection) : State = {
        val s = conn.getLastModified match {case 0 => state; case lms : Long => state.withLastModified(lms)}
        conn.getHeaderField("ETag") match {case null => s; case etag: String => s.withETag(etag)}
    }

    private def retrieveHttpFeed(state : State, url : URL) : (Response, State) = {
        val conn : HttpURLConnection = (url.openConnection).asInstanceOf[HttpURLConnection]
        // TODO set headers from state
        state.etag match {case Some(etag) => conn.setRequestProperty("If-None-Match", etag); case None => ;}
        state.lastModified match {case Some(lastmodified) => conn.setIfModifiedSince(lastmodified); case None => ;}
        // TODO conn.setConnectTimeout(int)
        // TODO conn.setReadTimeout(int)
        conn.setInstanceFollowRedirects(true)
        conn.setAllowUserInteraction(false)
        conn.connect()
        val now = (new Date).getTime
        try {
            val response : Response = conn.getResponseCode match {
                case 410 /* Gone, give up */ => new Response(PollResult.Gone, None)
                case 404 /* Not found */ => new Response(PollResult.NotFound, None)
                case 500 /* Server error */ => new Response(PollResult.Error, None)
                case 403 /* Forbidden, give up */ => new Response(PollResult.Forbidden, None)
                case 304 /* Not modified */ => new Response(PollResult.NotModified, None)
                case other : Int if (Subscription.ContentBearingResponseCodes.contains(other)) => /* OKs though redirects */
                    new Response(PollResult.OK, Some(readFeed(conn)))
                case _ => new Response(PollResult.Error, None) /* Cop out */
            }
            val newState = conn.getResponseCode match {
                case 301 => state.withNewURL(conn.getURL toString).updatedAt(now, response.result)
                case other : Int if (Subscription.ContentBearingResponseCodes.contains(other)) =>
                    extractConditionalHeaders(state, conn)
                case _   => state.updatedAt(now, response.result)
            }
            (response, newState)
        }
        catch {
            case err : Throwable => {
                    log.error(err)
                    val response = new Response(PollResult.Error, None)
                    val newState = state.updatedAt(now, response.result)
                    (response, newState)
            }
        }
        finally {
            conn.disconnect
        }
    }


    private def retrieveOtherFeed(state : State, url : URL) : (Response, State) = {
        val conn = url.openConnection
        val now = (new Date).getTime
        val response = try {
            val feed = readFeed(conn)
            new Response(PollResult.OK, Some(feed))
        }
        catch {
            case err : Throwable => {
                    log.error(err)
                    new Response(PollResult.Error, None) // TODO could look at causes ..
            }
        }
        (response, state.updatedAt(now, response.result))
    }

    private def readFeed(connection : URLConnection) : SyndFeed = {
        val in : InputStream = null
        try {
            val in = connection.getInputStream
            val xmlin = new XmlReader(in, true) // TODO content type? GZIP?
            val syndFeedInput = new SyndFeedInput
            syndFeedInput.build(xmlin)
        }
        finally {
            if (in != null) in.close()
        }
    }

    def poll(state: State) {
        log.debug("Polling: " + state.currentUrl)
        actor {
            val url = new URL(state.currentUrl)
            val (result, newstate) = url getProtocol match {
                case "http" => retrieveHttpFeed(state, url)
                case "https" => retrieveHttpFeed(state, url)
                case _ => retrieveOtherFeed(state, url)
            }
            this ! new Retrieved(result, newstate)
        }
    }

    def maybePublish(content : Response, newState : State) {
        log.debug(newState toString)
        saveState(newState)
        content.result match {
            case PollResult.OK => content.feed match {
                    case Some(syndfeed) => shredAndPublish(syndfeed)
                    case None => log.warn("Feed expected but not present " + content toString)
            }
            case res => log.debug("No feed returned (" + res.toString + ") ; doing nothing")
        }
    }

    def shredAndPublish(feed : SyndFeed) {
        feed.setFeedType("atom_1.0")
        val out = new SyndFeedOutput()
        val dom = out.outputJDom(feed)
        val entries = dom.getDescendants(new ElementFilter("entry"))
        val outputter = new XMLOutputter()
        while (entries.hasNext) {
            val entry = entries.next
            val xml = outputter.outputString(entry.asInstanceOf[Element])
            publish(xml)
        }
    }

    def setAlarm(seconds : Long) : ScheduledFuture[AnyRef] = {
        log.debug("Setting alarm for " + seconds + " seconds")
        val res = ActorPing.schedule(this, RetrieveNow, seconds * 1000)
        res
    }

    private val firstAlarm = setAlarm(0) // TODO set for lastUpdated + interval
    def act = loop(firstAlarm, initialState)
}
