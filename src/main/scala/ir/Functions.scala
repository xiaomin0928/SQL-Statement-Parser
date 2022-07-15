package ir

trait Function extends Expression

case class ExpressionFunction(functionName: String,
                              expressions: Option[Array[Expression]],
                              distinctExpressions: Option[Array[Expression]],
                              filterClause: Option[Selection],
                              overClause: Option[OverClause]
                             ) extends Function {
  override def emit = s"""$functionName (${if(distinctExpressions.isEmpty){s"""${if(expressions.isEmpty){}else{expressions.mkString(", ")}}"""}else{}}) $filterClause $overClause"""
  def toIR = s"""ExpressionFunction(functionName = $functionName,
                |                              expressions = {${expressions.mkString(", ")}},
                |                              distinctExpressions: {${distinctExpressions.mkString(", ")}},
                |                              filterClause: $filterClause,
                |                              overClause: $overClause
                |                             )""".stripMargin
}

case class AsteriskFunction(functionName: String,
                            filterClause: Option[Selection],
                            overClause: Option[OverClause]
                           ) extends Function {
  override def emit = s"""$functionName (*) $filterClause $overClause"""
  def toIR = s"""AsteriskFunction(functionName = $functionName,
                |                            filterClause = $filterClause,
                |                            overClause = $overClause
                |                           )""".stripMargin
}

case class EmptyFunction(functionName: String,
                         filterClause: Option[Selection],
                         overClause: Option[OverClause]
                        ) extends Function {
  override def emit = s"""$functionName () $filterClause $overClause"""
  def toIR = s"""EmptyFunction(functionName = $functionName,
                |                         filterClause = $filterClause,
                |                         overClause = $overClause
                |                        ) """.stripMargin
}

// 
case class OverClause(windowName: Option[String],
                      windowDefinition: Option[WindowDefinition]
                     ) extends Node {
  override def emit = s"""OVER ${if(windowName.isEmpty){s"""${if(windowDefinition.isEmpty){s"""()"""}else{s"""$windowDefinition"""}}"""}else{}}"""
  def toIR = s"""OverClause(windowName = $windowName,
                |                      windowDefinition = $windowDefinition
                |                     )""".stripMargin
    //"OVER " + (if windowName != None then windowName.get else windowDefn.get.toString)
}
