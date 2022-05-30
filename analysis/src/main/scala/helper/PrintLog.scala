package helper

import helper.LoggingLevel.LoggingLevel

object PrintLog {
  private var loggingLevel: LoggingLevel = LoggingLevel.Verbose
  private def logWithLevel(message: Any, loggingLevel: LoggingLevel): Unit = {
    if (loggingLevel >= this.loggingLevel) println(message)
  }

  def setLoggingLevel(loggingLevel: LoggingLevel): Unit = this.loggingLevel =
    loggingLevel
  def mute(): Unit = setLoggingLevel(LoggingLevel.Muted)

  def d(message: Any): Unit = logWithLevel(message, LoggingLevel.Debug)
  def v(message: Any): Unit = logWithLevel(message, LoggingLevel.Verbose)
  def i(message: Any): Unit = logWithLevel(message, LoggingLevel.Information)
  def e(message: Any): Unit = logWithLevel(message, LoggingLevel.Error)
}

object LoggingLevel extends Enumeration {
  type LoggingLevel = Value
  val Debug : Value = Value(0)
  val Verbose: Value = Value(1)
  val Information: Value = Value(2)
  val Error: Value = Value(3)
  val Muted: Value = Value(4)
}
