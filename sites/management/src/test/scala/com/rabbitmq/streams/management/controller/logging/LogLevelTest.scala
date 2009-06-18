package net.lshift.feedshub.management.controller.logging

import org.scalatest.junit.JUnit3Suite

class LogLevelTest extends JUnit3Suite {
  private val fatal: LogLevel = Fatal
  private val error: LogLevel = Error

  def testAndUp() = {
    expect(Set(Fatal)) {fatal.andUp}
    expect(Set(Fatal, Error)) {error.andUp}
  }

  def testAndDown() = {
    expect(Set(Fatal, Debug, Error, Info, Warn)) {fatal.andDown}
    expect(Set(Error, Debug, Info, Warn)) {error.andDown}
  }
  
  def testFrom() = {
    expect(Fatal) {LogLevel.from("fatal")}
  }

  def testFromWithBadData() = {
    intercept[scala.MatchError] {LogLevel.from("wrong")}
  }
}