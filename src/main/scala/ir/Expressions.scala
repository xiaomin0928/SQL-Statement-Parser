package ir

trait Expression extends Node {}

trait LiteralValueExpression extends Expression

case class ColumnExpression(columnName: (String,String)
                           ) extends Expression {
  override def emit = s"""${if(columnName._1.isEmpty){s"""${columnName._2}"""}else{s"""${columnName._1}/${columnName._2}"""}}"""
  def toIR = s"""ColumnExpression(columnName = $columnName)"""
}

case class UnaryOperatorExpression(unaryOperator: UnaryOperatorEnum,
                                   expression: Expression
                                  ) extends Expression {
  override def emit = s"""$unaryOperator $expression"""
  def toIR = s"""UnaryOperatorExpression(unaryOperator = $unaryOperator, expression= $expression )"""
}

enum UnaryOperatorEnum {
  case positive, negative

  override def toString: String = this match {
    case UnaryOperatorEnum.positive => "+"
    case UnaryOperatorEnum.negative => "-"
  }
}

//var BinaryOperatorEnumSymbol = Array(" + " , " - " , " * " , " / " ," % ", " = " , " != " ,  " > " , " < " , " >= " , " <= " ," ! ", " & " , " | ") // tbd

case class BinaryOperatorExpression(leftExpression: Expression,
                                    binaryOperator: BinaryOperatorEnum,
                                    rightExpression: Expression
                                   ) extends Expression {
  override def emit: String = s"""$leftExpression $binaryOperator $rightExpression""".stripMargin
  def toIR = s"""BinaryOperatorExpression(leftExpression = $leftExpression, binaryOperator = $binaryOperator, rightExpression = $rightExpression )"""
}

enum BinaryOperatorEnum extends Expression {
  case add, subtract, multiply, divide, modulo, equalTo, notEqualTo,notequalTo,
  greaterThan, lessThan, greaterThanOrEqualTo, lessThanOrEqualTo, not, and, or,AND,OR

  override def emit: String = this match {
    case BinaryOperatorEnum.add => "+"
    case BinaryOperatorEnum.subtract => "-"
    case BinaryOperatorEnum.multiply => "*"
    case BinaryOperatorEnum.divide => "/"
    case BinaryOperatorEnum.modulo => "%"
    case BinaryOperatorEnum.equalTo => "="
    case BinaryOperatorEnum.notEqualTo => "!="
    case BinaryOperatorEnum.notequalTo => "<>"
    case BinaryOperatorEnum.greaterThan => ">"
    case BinaryOperatorEnum.lessThan => "<"
    case BinaryOperatorEnum.greaterThanOrEqualTo => ">="
    case BinaryOperatorEnum.lessThanOrEqualTo => "<="
    case BinaryOperatorEnum.not => "!"
    case BinaryOperatorEnum.and => "&"
    case BinaryOperatorEnum.or => "|"
    case BinaryOperatorEnum.AND => "AND"
    case BinaryOperatorEnum.OR => "OR"
  }
}

case class Expressions(expressions: Array[Expression]
                      ) extends Expression {
  override def emit = s"""${expressions.mkString(" ")}"""
  def toIR = s""" Expressions(expressions =  {${expressions.mkString(", ")}} )"""
}

case class CastExpression(castExpression: Expression,
                          typeName: String
                         ) extends Expression {
  override def emit = s"""CAST ($castExpression AS $typeName)"""
  def toIR = s""" CastExpression(castExpression= $castExpression ,typeName = $typeName)"""
}

case class TypeName(names: Array[String],
                    signedNumbers: Option[Array[SignedNumber]]
                   ) extends Node {
  override def emit = s"""${names.mkString(" ")} ${if(signedNumbers.isDefined){signedNumbers.mkString(" , ")}else{""} }"""
  def toIR = s""" TypeName(names = {${names.mkString(", ")}}, signedNumbers= ${if(signedNumbers.isDefined){signedNumbers.mkString(" , ")}else{""} })"""
    //names.mkString(" ") + (if(!signedNumbers.isEmpty){signedNumbers.mkString(" , ")}else{""})
}

case class SignedNumber(sign: Option[SignEnum],
                        literal: IntegerLiteral|DoubleLiteral
                       ) extends Node {
  override def emit = s"""(${if(sign.isDefined){s"""$sign"""}else{""}} $literal) """
  def toIR = s""" SignedNumber(sign = ${if(sign.isDefined){s"""$sign"""}else{" "}},literal = $literal)"""
}

enum SignEnum {
  case plus, minus

  override def toString: String = this match {
    case SignEnum.plus => "+"
    case SignEnum.minus => "-"
  }
}
  

//var SignSymbol = Array(" + " , " - ") // tbd

case class CollateExpression(collateExpression: Expression,
                             collationName: String
                            ) extends Expression {
  override def emit = s"""$collateExpression COLLATE $collationName"""
  def toIR = s""" CollateExpression(collateExpression= $collateExpression, collationName = $collationName )"""
}

case class LikeExpression(expression: Expression,
                          negation: Boolean,
                          likeExpression: Expression,
                          escapeExpression: Option[Expression]
                         ) extends Expression {
  override def emit = s"""$expression ${if(negation){}else{s"""NOT"""}} LIKE $likeExpression ${if(escapeExpression.isDefined){s"""ESCAPE $escapeExpression"""}else{}}"""
  def toIR = s"""LikeExpression(expression= $expression,negation = $negation,likeExpression = $likeExpression,escapeExpression = $escapeExpression)"""
  //expression.toString + (if(negation == true){""}else{" NOT "})
    //+ " LIKE " + likeExpression.toString + (if(!escapeExpression.isEmpty){" ESCAPE " + escapeExpression.toString}else{""})
}

case class GlobExpression(expression: Expression,
                          negation: Boolean,
                          globExpression: Expression
                         ) extends Expression {
  override def emit = s"""$expression ${if(negation){}else{s"""NOT"""}} GLOB $globExpression"""
  def toIR = s"""GlobExpression(expression = $expression, negation: $negation, globExpression = $globExpression)"""
  ///*expression.toString +*/ (if(negation == true){""}else{" NOT "}) + " GLOB " + globExpression.toString
}

case class RegexpExpression(expression: Expression,
                            negation: Boolean,
                            regexpExpression: Expression
                           ) extends Expression {
  override def emit = s"""$expression ${if(negation){}else{s"""NOT"""}} REGEX $regexpExpression"""
  def toIR = s"""RegexpExpression(expression = $expression, negation: $negation, regexpExpression = $regexpExpression)"""
  ///*expression.toString +*/ (if(negation == true){""}else{" NOT "}) + " GLOB " + regexpExpression.toString
}

case class MatchExpression(expression: Expression,
                           negation: Boolean,
                           matchExpression: Expression
                          ) extends Expression {
  override def emit = s"""$expression ${if(negation){}else{s"""NOT"""}} MATCH $matchExpression"""
  def toIR = s"""MatchExpression(expression = $expression, negation: $negation, MatchExpression = $MatchExpression)"""
  ///*expression.toString +*/ (if(negation == true){""}else{" NOT "}) + " GLOB " + matchExpression.toString
}

case class NullExpression(expression: Expression,
                          nullType: NullTypeEnum,
                         ) extends Expression {
  override def emit = s"""$nullType $expression"""
  def toIR = s"""NullExpression(expression = $expression, nullType: $nullType)"""
    //nullType.toString + expression.toString
}

enum NullTypeEnum {
  case isNull, notNull, notnull

  override def toString: String = this match {
    case NullTypeEnum.isNull => "IS NULL"
    case NullTypeEnum.notNull => "NOT NULL"
    case NullTypeEnum.notnull => "NOTNULL"
  }
}

case class IsExpression(expression: Expression,
                        negation: Boolean,
                        isExpression: Expression,
                       ) extends Expression {
  override def emit = s""" $expression IS ${if(negation){}else{s"""NOT"""}} $isExpression"""
  def toIR = s"""IsExpression(expression = $expression,  negation = $negation, isExpression: $isExpression)"""
    //leftExpression.toString + " IS " +(if(negation == true){""}else{" NOT "}) + rightExpression.toString
}

case class BetweenExpression(expression: Expression,
                             negation: Boolean,
                             betweenExpression: Expression,
                             andExpression: Expression,
                            ) extends Expression {
  override def emit = s"""$expression ${if(negation){}else{s"""NOT"""}} BETWEEN $betweenExpression AND $andExpression"""
  def toIR = s"""BetweenExpression(expression = $expression, negation = $negation, betweenExpression = $betweenExpression, andExpression = $andExpression)"""
    //expression.toString  + (if(negation == true){""}else{" NOT "}) + " BETWEEN " + betweenExpression.toString + " AND " + andExpression.toString
}

case class InExpression(expression: Expression,
                        negation: Boolean,
                        selectStatement: Option[SelectStatement],
                        expressions: Option[Array[Expression]]
                       ) extends Expression {
  override def emit: String =s"""$expression ${if(negation){s"""IN"""}else{s"""NOT IN"""}}
       |${if(selectStatement.isDefined){s"""($selectStatement)"""}
        else{if(expressions.isDefined){s"""( ${expressions.mkString(" , ")} )""" }else{s"""()"""}} } """.stripMargin
  def toIR = s"""InExpression(expression = $expression,
                |                        negation = $negation,
                |                        selectStatement = $selectStatement,
                |                        expressions = {${expressions.mkString(", ")}}
                |                       )""".stripMargin

  //expression.toString  + (if(negation == true){" IN "}else{" NOT IN "}) +
    //(if(!selectStatement.isEmpty){"(" + selectStatement.toString + ")"}else{ (if(!expressions.isEmpty){"(" +expressions.mkString(" , ") + ")" }else{"()"})})
}

case class SelectExpression(selectType: Option[SelectTypeEnum],
                            selectStatement: SelectStatement
                           ) extends Expression {
  override def emit = s"""$selectType ($selectStatement)"""
  def toIR = s"""SelectExpression(selectType = $selectType, selectStatement = $selectStatement )""".stripMargin
    //(if(!selectType.isEmpty){selectType.toString}else{""}) + "(" + selectStatement.toString + ")"
}

enum SelectTypeEnum {
  case exists, not, notExists
  
  override def toString: String = this match {
    case SelectTypeEnum.exists => "EXISTS"
    case SelectTypeEnum.not => "NOT"
    case SelectTypeEnum.notExists => "NOT EXISTS"
  }
}

case class CaseExpression(caseExpression: Option[Expression],
                          whenExpression: Array[Expression],
                          thenExpression: Array[Expression],
                          elseExpression: Option[Expression]
                         ) extends Expression {
  override def emit: String =s"""
       |$caseExpression
       |WHEN ${whenExpression.mkString(" ")}
       |THEN ${thenExpression.mkString(" ")}
       |${if(elseExpression.isDefined){s"""ELSE  $elseExpression"""}else{""}}""".stripMargin
  def toIR = s"""CaseExpression(caseExpression = $caseExpression,
                |                          whenExpression = $whenExpression,
                |                          thenExpression = $thenExpression,
                |                          elseExpression = $elseExpression
                |                         )""".stripMargin
    //caseExpression.getOrElse("").toString + " WHEN " + whenExpression.toString + " THEN " + thenExpression.toString + (if(!elseExpression.isEmpty){" ELSE " + elseExpression.getOrElse("")}else{""})
}

case class RaiseFunction(raiseType: RaiseTypeEnum,
                         errorMessage: Option[String]
                        ) extends Expression {
  override def emit = s""" RAISE ($raiseType ${if(errorMessage.isDefined){s""",$errorMessage"""}else{s""""""}})"""
  def toIR = s"""RaiseFunction(raiseType = $raiseType,
                |                         errorMessage = $errorMessage
                |                        )""".stripMargin
    /*" RAISE (" + raiseType.toString + (if(!errorMessage.isEmpty){"," + errorMessage}else{""})*/
}

enum RaiseTypeEnum {
  case ignore, rollback, abort, fail

  override def toString: String = this match {
    case RaiseTypeEnum.ignore => "IGNORE"
    case RaiseTypeEnum.rollback => "ROLLBACK"
    case RaiseTypeEnum.abort => "ABORT"
    case RaiseTypeEnum.fail => "FAIL"
  }
}