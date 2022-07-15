package ir

// Projection:
case class Projection(expression: Option[Expression],
                      alias: Option[String],
                      asterisk: Option[Boolean]
                     ) extends Node {
  override def emit = s"""${if(asterisk.isEmpty){s"""${expression.get} ${if(alias.isEmpty){s""""""}else{s"""AS ${alias.get}"""}}"""}else{s"""*"""}} """
  def toIR = s"""Projection(expression = $expression,
                |                      alias = $alias,
                |                      asterisk = $asterisk
                |                     )""".stripMargin
}

// Selection:
case class Selection(expression: Expression
                    ) extends Node {
  override def emit = s"""$expression"""
  def toIR = s"""Selection(expression = expression
                |                    )""".stripMargin
}

// GroupBy:
case class GroupBy(groupExpressions: Array[Expression],
                   havingExpression: Option[Expression]
                  ) extends Node {
  override def emit = s"""${groupExpressions.mkString(" ")} ${if(havingExpression.isDefined){s""" HAVING $havingExpression"""}else{""}}"""
  def toIR = s"""GroupBy(groupExpressions = {${groupExpressions.mkString(" ")}},
                |                   havingExpression = $havingExpression
                |                  )""".stripMargin
    //groupExpressions.mkString(" ") + (if(!havingExpression.isEmpty){" HAVING " + havingExpression.getOrElse("")}else{""})
}

// OrderBy:
case class OrderBy(orderingTerms: Array[OrderingTerm]
                  ) extends Node {
  override def emit = s"""${orderingTerms.mkString(" ")}"""
  def toIR = s"""OrderBy(orderingTerms = {${orderingTerms.mkString(" ")}}
                |                  )""".stripMargin
}

// Limit:
case class Limit(limitExpression: Expression,
                 offsetExpression: Option[Expression],
                ) extends Node {
  override def emit = s"""$limitExpression ${if(offsetExpression.isDefined){s"""OFFSET $offsetExpression"""}else{""}}"""
  def toIR = s"""Limit(limitExpression = $limitExpression,
                |                 offsetExpression = $offsetExpression,
                |                )""".stripMargin
}

// OrderingTerm:
case class OrderingTerm(expression: Expression,
                        collationName: Option[String],
                        order: Option[OrderEnum],
                        nullOrder: Option[NullOrderEnum]
                       ) extends Node {
  override def emit = s"""$expression $collationName $order $nullOrder"""
  def toIR = s"""OrderingTerm(expression = $expression,
                |                        collationName = $collationName,
                |                        order = $order,
                |                        nullOrder = $nullOrder
                |                       )""".stripMargin
}

enum OrderEnum {
  case asc, desc
  
  override def toString: String = this match {
    case OrderEnum.asc => "ASC"
    case OrderEnum.desc => "DESC"
  }
}

enum NullOrderEnum {
  case nullsFirst, nullsLast
  
  override def toString: String = this match {
    case NullOrderEnum.nullsFirst => "NULLS FIRST"
    case NullOrderEnum.nullsLast => "NULLS LAST"
  }
}

// CompoundOperator
case class CompoundOperator(compoundOperators: Array[CompoundOperatorEnum]
                           ) extends Node {
  override def emit = s"""${compoundOperators.mkString(" , ")}"""
  def toIR = s"""CompoundOperator(compoundOperators = {${compoundOperators.mkString(", ")}}
                |                           )""".stripMargin
}

enum CompoundOperatorEnum extends Node {
  case union, unionAll, intersect, except

  override def emit: String = this match {
    case CompoundOperatorEnum.union => "UNION"
    case CompoundOperatorEnum.unionAll => "UNION ALL"
    case CompoundOperatorEnum.intersect => "INTERSECT"
    case CompoundOperatorEnum.except => "EXCEPT"
  }
}




