
//***************************************************************************************
//
// Title:   Main
// Author:  Xiaomin Wang, Alex Schuler, Shue Kwan Ip, Ahmet Kösker
// Date:    January 25th, 2022
//
//***************************************************************************************

import ir.*

import scala.util.parsing.combinator.syntactical.*
import scala.util.parsing.input.CharSequenceReader

object Test {

  def main(args: Array[String]): Unit = {
    val query =           """
                            |SELECT
                            |    product/orderdate,
                            |    lineitem/discount
                            |WHERE
                            |    (product/orderdate BETWEEN '1995-01-01' AND '1996-12-31')
                            |    OR (lineitem/discount NOT BETWEEN (2 - 0.01) AND (2 + 0.01))
                            |""".stripMargin

    Parser(query) match {
      case Right(query) => println(s"Parsed SQL statement is:\n  $query")
      case Left(value) => println(s"No statement parsed! ($value)")
    }
  }

}
