package net.lshift.feedshub.management.controller

import org.scalatest.junit.JUnit3Suite

class LogBindingTest extends JUnit3Suite {
  private val wildcard = "#"
  private val separator = "."
  private val binding = LogBinding()

  def testDefaultConstruction = {
    expect(1) {binding.bindings.size}
    expect(Set(wildcard)) {binding.bindings}
  }

  def testWithAny = {
    val b = binding.withAny

    expect(1) {b.bindings.size}
    expect(Set(wildcard)) {b.bindings}
  }

  def testWithLogLevelAbove = {
    expect(withComponentWildCard(Set[LogLevel](Fatal))) {binding.withLogLevelAbove(Fatal).bindings}
    expect(withComponentWildCard(Set(Fatal, Error))) {binding.withLogLevelAbove(Error).bindings}
    expect(Set(wildcard)) {binding.withLogLevelAbove(Debug).bindings}
  }

  def testWithLogLevelBelow = {
    expect(withComponentWildCard(Set[LogLevel](Debug))) {binding.withLogLevelBelow(Debug).bindings}
    expect(Set(wildcard)) {binding.withLogLevelBelow(Fatal).bindings}
  }

  private def withComponentWildCard(levels: Set[LogLevel]): Set[String] = levels.map(withComponentWildCard(_))
  private def withComponentWildCard(level: LogLevel): String = level.stringValue + separator + wildcard

  def testWithComponents = {
    val components = List("x", "y", "z")
    expect(Set(componentsToString(wildcard, components))) {binding.withComponents(components).bindings}
    expect(Set(componentsToString(Fatal.stringValue, components))) {binding.withComponents(components).withLogLevelAbove(Fatal).bindings}
    expect(Set(componentsToString(Warn.stringValue, components), componentsToString(Info.stringValue, components), componentsToString(Debug.stringValue, components))) {binding.withComponents(components).withLogLevelBelow(Warn).bindings}

    expect(Set(componentsToString(wildcard, components))) {binding.withComponents(components).withLogLevelBelow(Fatal).bindings}
    expect(Set(componentsToString(wildcard, components))) {binding.withComponents(components).withLogLevelAbove(Debug).bindings}
  }
  
  private def componentsToString(level: String, components:List[String]) = (List(level) ::: components).mkString(separator)
}