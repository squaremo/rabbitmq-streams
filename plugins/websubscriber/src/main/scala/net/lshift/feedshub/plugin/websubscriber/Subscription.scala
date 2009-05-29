/*
 * Subscription.scala
 */

package net.lshift.feedshub.plugin.websubscriber

import scala.actors.Actor
import scala.actors.Actor._

import net.sf.json._

import net.lshift.feedshub.harness.Logger

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
case object StopPolling
case class Retrieved(content: Response, state : State)

case class Response(result: PollResult.PollResult, feed: Option[SyndFeed])

object PollResult extends Enumeration("ok", "notfound", "gone", "accessdenied", "error") {
    type PollResult = Value
    val OK, NotFound, Gone, AccessDenied, Error = Value
}

case class State(currentUrl : String, originalUrl : String, lastUpdated : Long, interval : Int, lastResult: PollResult.PollResult)

class Subscription(log : Logger, initialState: State, saveState: State => Unit, publish: String => Unit) extends Actor {

    def loop(alarm: ScheduledFuture[AnyRef],  state: State) {
        react {
            case RetrieveNow => poll(state); loop(setAlarm(state.interval), state)
                // don't try to interrupt something that's already running,
                // just to avoid complication
            case StopPolling => alarm.cancel(false)
            case Retrieved(content, responseState) => maybePublish(content, responseState); loop(alarm, responseState)
        }
    }

    private def retrieveHttpFeed(state : State, url : URL) : (Response, State) = {
        val conn : HttpURLConnection = (url openConnection).asInstanceOf[HttpURLConnection]
        // TODO set headers from state
        // TODO conn.setConnectTimeout(int)
        // TODO conn.setReadTimeout(int)
        conn.setInstanceFollowRedirects(true)
        conn.connect()
        val now = (new Date).getTime
        try {
            // See http://diveintomark.org/archives/2003/07/21/atom_aggregator_behavior_http_level
            // for the ideal
            val response : Response = conn.getResponseCode match {
                case 410 /* Gone */ => new Response(PollResult.Gone, None)
                case 404 /* not found */ => new Response(PollResult.NotFound, None)
                case 500 /* server error */ => new Response(PollResult.Error, None)
                case 403 /* Forbidden, give up */ => new Response(PollResult.Gone, None)
                case other : Int if (other >= 200 && other < 400) => /* OKs though redirects */
                    new Response(PollResult.OK, Some(readFeed(conn)))
                case _ => new Response(PollResult.Error, None) /* Cop out */
            }
            val newState = conn.getResponseCode match {
                case 301 => new State(conn.getURL toString, state.originalUrl, now, state.interval, response.result)
                case _   => new State(state.currentUrl, state.originalUrl, now, state.interval, response.result)
            }
            (response, newState)
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
            case _ => new Response(PollResult.Error, None) // TODO could look at causes ..
        }
        (response, new State(state.originalUrl, state.currentUrl, now, state.interval, response.result))
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
            // Actually go and get the thing;
            // then make an updated state and pass it back

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
        //log.debug("Set alarm")
        res
    }

    private val firstAlarm = setAlarm(0) // make sure we set the alarm
    def act = loop(firstAlarm, initialState)
}
