package com.rabbitmq.streams.management.controller.logging

class LogBinding private(val lowest: Option[LogLevel], val highest: Option[LogLevel], val components: Seq[String]) {
  val bindings: Set[String] = {
    val ups: Set[LogLevel] = lowest match {
      case Some(level) => level.andUp
      case None => Set()
    }
    val downs: Set[LogLevel] = highest match {
      case Some(level) => level.andDown
      case None => Set()
    }

    val levels: Set[LogLevel] = Set.empty ++ ups ++ downs
    if(!levels.isEmpty) {
      levels.map(withComponentString(_))
    }
    else {
      Set(reduceWildcards(LogBinding.zeroOrMoreWildCard + LogBinding.seperator + componentString))
    }
  }

  def withAny: LogBinding = new LogBinding(None, None, Seq.empty)

  def withLogLevelBelow(level: LogLevel) = level match {
    case Fatal => new LogBinding(None, None, components)
    case _ => new LogBinding(None, Some(level), components)
  }

  def withLogLevelAbove(level: LogLevel) = level match {
    case Debug => new LogBinding(None, None, components)
    case _ => new LogBinding(Some(level), None, components)
  }

  private def withComponentString(l: LogLevel): String = reduceWildcards(l.stringValue + LogBinding.seperator + componentString)

  private def reduceWildcards(s: String): String = if(s.exists(c => c != '#' && c != '.')) s else LogBinding.zeroOrMoreWildCard

  private def componentString: String = if(components.isEmpty) LogBinding.zeroOrMoreWildCard else components.mkString(LogBinding.seperator) + LogBinding.seperator + LogBinding.zeroOrMoreWildCard

  def withComponents(cs: Seq[String]) = new LogBinding(lowest, highest, cs)

}

object LogBinding {
  private val seperator = "."
  private val zeroOrMoreWildCard = "#"

  def apply() = new LogBinding(None, None, Seq.empty)
  def Any = LogBinding()
}
