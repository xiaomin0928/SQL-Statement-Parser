package ir

import java.time.{LocalDate, LocalTime}
import java.nio.charset.StandardCharsets

case class IntegerLiteral(value: Int) extends LiteralValueExpression {
  override def emit = s"$value"
  def toIR = s"""IntegerLiteral(value = $value)""".stripMargin
}

case class DoubleLiteral(value: Double) extends LiteralValueExpression {
  override def emit = s"$value"
  def toIR = s"""DoubleLiteral(value = $value)""".stripMargin
}

case class StringLiteral(value: String) extends LiteralValueExpression {
  override def emit = s"""\"$value\""""
  def toIR = s"""StringLiteral(value = $value)""".stripMargin
}

case class BlobLiteral(bytes: Array[Byte]) extends LiteralValueExpression {
  override def emit = new String(bytes, StandardCharsets.UTF_8)
  def toIR = s"""BlobLiteral(bytes = ${String(bytes, StandardCharsets.UTF_8)})""".stripMargin
}

case class BooleanLiteral(value: Boolean) extends LiteralValueExpression {
  override def emit = s"$value"
  def toIR = s"""BooleanLiteral(value = $value)""".stripMargin
}

case class NullLiteral() extends LiteralValueExpression {
  override def emit = "NULL"
  def toIR = s"""NullLiteral()""".stripMargin
}

case class DateLiteral(value: LocalDate) extends LiteralValueExpression {
  override def emit = s"$value"
  def toIR = s"""DateLiteral(value = $value)""".stripMargin
}

case class TimeLiteral(value: LocalTime) extends LiteralValueExpression {
  override def emit = s"$value"
  def toIR = s"""TimeLiteral(value = $value)""".stripMargin
}

case class DateTimeLiteral(date: LocalDate,
                           time: LocalTime) extends LiteralValueExpression {
  override def emit = s"$time $date"
  def toIR = s"""DateTimeLiteral(date = $date,
                |                           time = $time)""".stripMargin
}
