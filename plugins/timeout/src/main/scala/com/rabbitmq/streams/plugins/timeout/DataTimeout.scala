package com.rabbitmq.streams.plugins.timeout

import scala.actors.Actor
import scala.actors.Actor._

import net.sf.json.JSONObject
import com.rabbitmq.streams.harness.PipelineComponent
import com.rabbitmq.streams.harness.InputReader
import com.rabbitmq.streams.harness.InputMessage
import com.rabbitmq.streams.harness.PluginBuildException
import com.rabbitmq.streams.harness.NotificationType
import java.util.Date

/**
 * A plugin that sends a notification when it hasn't seen data for the configured
 * time.  Compose with deduplication to alert when there's no different data; compose with
 * dispatch to alert when there's no matching data.
 *
 */
object DataTimeout {
  case class Timeout()
  case class Input(msg : InputMessage)
  case object Stop
}
class DataTimeout extends PipelineComponent with Actor {

  import DataTimeout.{Timeout, Input, Stop}

  private var timeout : Int = 0 // milliseconds
  private var message = ""

  private var state : java.util.Map[String, Object] = null

  def now : Long = {
    new Date().getTime
  }

  override def configure(config : JSONObject) {
    if (!config.containsKey("timeout") || !config.containsKey("message")) {
      throw new PluginBuildException("Need both of timeout and message in the configuration")
    }

    try {
      timeout = config.get("timeout").asInstanceOf[Int]
      message = config.get("message").asInstanceOf[String]
    }
    catch {
      case ex : Exception => throw new PluginBuildException("Unable to interpret configuration values given -- check the types", ex)
    }
    if (timeout <= 0) throw new PluginBuildException("A zero or negative timeout doesn't make sense")

    state = getState

    val outer = this
    object input extends InputReader {
      override def handleMessage(msg : InputMessage, ignored : JSONObject) {
        outer ! new Input(msg)
      }
    }
    registerInput("input", input)

    getAlarmState match {
      case None => {
          val alarm = now + timeout
          setAlarmState(alarm)
          setTimer(alarm)
        }
      case Some(a) => {
          maybeTriggerTimer()
        }
    }

    // start on configuration; we won't actually get anything until
    // the harness hooks consumers up.  Or we time out.
    // Note that we can't just treat the thread as an actor; we have to return
    // from the constructor and from configure.
    start
  }

  def loop() {
    receive {
      case Timeout => maybeTriggerTimer; loop
      case Input(msg) => publishToChannel("output", msg); setAlarmState(now + timeout); loop
      case Stop =>; // don't loop
    }
  }

  def maybeTriggerTimer() {
    getAlarmState match {
      case None => log.error("Timeout without an alarm set"); // We timed out, but we don't have an alarm recorded.  Hrm.
      case Some(alarm) => {
        val n = now
        log.debug("It's now " + n + " and I'm supposed to wake up at " + alarm)
        if (alarm <= n) {
          ((n - alarm) / timeout) match {
            case x if (x > 1) =>
              log.warn(x + " timeouts missed since " +alarm+ "; notifying once.")
            case _ =>;
          }
          notifier.notify(NotificationType.NoData, message)
          val newalarm = n + timeout
          setAlarmState(newalarm)
          setTimer(newalarm)
        } else {
          // We got prodded, but it turns out there's been messages in the meantime.
          setTimer(alarm)
        }
      }
    }
  }

  def getAlarmState : Option[Long] = {
    state.get("alarm") match {
      case null => None
      case a => Some(a.asInstanceOf[Long])
    }
  }
  def setAlarmState(time : Long) {
    log.debug("Setting alarm for " + time.toString)
    state.put("alarm", new java.lang.Long(time))
    setState(state)
    // setState avoids conflicts, so we don't need to get here as well
  }

  def setTimer(time : Long) {
    val sleepTime : Long = (time - now) match {
      case inThePast if (inThePast <= 0) => {
          // This may occur because the timeout is smaller than the time it takes to
          // get here from where it was invoked.  If we let this kind of behaviour
          // continue, it'll quite possibly thrash; so we set an arbitrary timeout
          // this time.
          log.warn("Race: we are trying to set an alarm in the past. Using now + 100ms instead")
          100
        }
      case s => s
    }
    // Note that we don't try and mess with this on an input; this is on the assumption
    // that the timeout will generally be much longer than the time between messages.
    val outer = this
    actor {
      Thread.sleep (sleepTime)
      outer ! Timeout
    }
  }

  def act = loop()

}
