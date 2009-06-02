package net.lshift.feedshub.management.controller.logging

class LogLevel {
  def andUp = LogLevel.downTo(this)

  def andDown = LogLevel.upFrom(this)

  def stringValue: String = {
    this match {
      case Debug => "DEBUG"
      case Info => "INFO"
      case Warn => "WARN"
      case Error => "ERROR"
      case Fatal => "FATAL"
    }
  }
}

object LogLevel extends LogLevel {
  val values = List(Fatal, Error, Warn, Info, Debug)

  def downTo(level: LogLevel): Set[LogLevel] = Set(values.takeWhile(_ != level): _*) + level

  def upFrom(level: LogLevel): Set[LogLevel] = Set(values.dropWhile(_ != level): _*)

  def from(s: String): LogLevel = {
    s.toLowerCase match {
      case "debug" => Debug
      case "info" => Info
      case "warn" => Warn
      case "error" => Error
      case "fatal" => Fatal
    }
  }
}

object Debug extends LogLevel
object Info extends LogLevel
object Warn extends LogLevel
object Error extends LogLevel
object Fatal extends LogLevel
