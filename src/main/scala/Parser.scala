
//***************************************************************************************
//
// Title:   SQL Parser
// Author:  Xiaomin Wang, Alex Schuler, Shue Kwan Ip, Ahmet Kösker
// Date:    January 25th, 2022
//
//***************************************************************************************

import ir.{Expression, *}

import java.time.*
import scala.util.matching.Regex
import scala.util.parsing.combinator.*
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.combinator.token.StdTokens
import scala.util.parsing.input.CharArrayReader.EofCh
import scala.util.parsing.input.CharSequenceReader

final private class ParserLexical extends StdLexical with StdTokens {

  override def token: Parser[Token] =
    ( identChar ~ rep( identChar | digit )   ^^ { case first ~ rest => processIdent(first :: rest mkString "") }
      | digit ~ rep( digit | '.')                 ^^ { case first ~ rest => NumericLit(first :: rest mkString "") }
      | '\'' ~> rep( chrExcept('\'', '\n') ) >> { chars => stringEnd('\'', chars) }
      | '\"' ~> rep( chrExcept('\"', '\n') ) >> { chars => stringEnd('\"', chars) }
      | EofCh                                ^^^ EOF
      | delim
      | failure("illegal character")
      )

  private def stringEnd(quoteChar: Char, chars: List[Char]): Parser[Token] = {
    { elem(quoteChar) ^^^ StringLit(chars mkString "") } | err("unclosed string literal")
  }

  override def processIdent(name: String): Token = {
    val upperCased = name.toUpperCase()
    if (reserved contains upperCased) Keyword(upperCased) else Identifier(name)
  }
}

final private class Parser extends StandardTokenParsers with PackratParsers {
  import lexical._
  type Tokens = StdTokens
  override val lexical = new ParserLexical

  lexical.reserved ++= keywords.ALL_KEYWORDS

  lexical.delimiters ++= Seq("+", "-", "*", "/", "=", "!=", "<>", ">", "<", ">=", "<=", "&", "|", "(", ")", "'", ",")

  def objectMapper(body: Object): String = {
    body match {
      case first ~ second => Array(first.toString, second.toString).mkString(" ").replaceAll(" +", " ")
      case w => w.toString
    }
  }

  def isColumnsExpression(expr: List[Expression]): Array[Expression] = {
    var column_array = Array.empty[Expression]
    for (e <- expr.toArray) {
      if (!e.isInstanceOf[ColumnExpression]) column_array = column_array :+ ColumnExpression(columnName = (e.toString.replaceAll("\"", ""), ""))
    }

    column_array
  }

  def regex(r: Regex): Parser[String] = acceptMatch(
    s"matching regex $r",
    { case lexical.Identifier(s) if r.unapplySeq(s).isDefined => s
      case lexical.Keyword(s) if r.unapplySeq(s).isDefined => s
      case lexical.NumericLit(s) if r.unapplySeq(s).isDefined => s
      case lexical.StringLit(s) if r.unapplySeq(s).isDefined => s
  }) | failure("regex mismatched")

  // https://www.w3schools.com/sql/func_sqlserver_cast.asp
  val datatypes = "(?:^|(?<= ))(" + keywords.DATATYPE.mkString("|") + ")(?:(?= )|$)"
  
  def datatypeParser: Parser[String] = regex(datatypes.r) ^^ {case s => s} | failure("illegal datatype")

  // Match any words, except the reserved keywords in SQL
  var words = "(?!\\b(" + keywords.ALL_KEYWORDS.mkString("|") + ")\\b)" + "[0-9a-zA-Z$_]+"

  // Ref : https://dev.mysql.com/doc/refman/8.0/en/number-literals.html
  def numericLiteralParser: Parser[IntegerLiteral|DoubleLiteral] = regex("""[-+]?[0-9]*\.?[0-9]+""".r) ^^ {
    case s => {
      if (s.contains(".")) DoubleLiteral(value = s.toDouble)
      else IntegerLiteral(value = s.toInt)
    }
  }

  // Ref : https://dev.mysql.com/doc/refman/8.0/en/identifiers.html
  def schemaObjectParser: Parser[String] = regex(words.r) ^^ {identity}
  def functionNameParser: Parser[String] = regex("[0-9a-zA-Z$_]+".r) ^^ {identity}

  // Ref : https://docs.microsoft.com/en-us/dotnet/framework/data/adonet/ef/language-reference/literals-entity-sql
  // def stringLiteralParser: Parser[StringLiteral] = regex("(N)?'(?:[^']|'')*'|\"(?:[^\"]|\"\")*\"".r) ^^ {case s => StringLiteral(value = s)}
  def stringLiteralParser: Parser[StringLiteral] = regex("(?s).*".r) ^^ {case s => StringLiteral(value = s)}

  // Ref : https://www.sqlitetutorial.net/sqlite-data-types/
  def blobLiteralParser: Parser[BlobLiteral] = regex("""x""".r) ~> "'" ~> schemaObjectParser <~ "'" ^^ {case s => println("heerere"); BlobLiteral(bytes = Array("x", "'", s, "'").mkString("").getBytes())}

  // Ref : https://www.sqlite.org/lang_expr.html (see remark 2)
  def binaryOperatorParser: Parser[BinaryOperatorEnum] = ("+" | "-" | "*" | "/" | "=" | "!=" | "<>" | ">" | "<" | ">=" | "<=" | "&" | "|" | "AND" | "OR") ^^ {
    case "+" => BinaryOperatorEnum.add
    case "-" => BinaryOperatorEnum.subtract
    case "*" => BinaryOperatorEnum.multiply
    case "/" => BinaryOperatorEnum.divide
    case "=" => BinaryOperatorEnum.equalTo
    case "!=" => BinaryOperatorEnum.notEqualTo
    case "<>" => BinaryOperatorEnum.notequalTo
    case ">" => BinaryOperatorEnum.greaterThan
    case "<" => BinaryOperatorEnum.lessThan
    case ">=" => BinaryOperatorEnum.greaterThanOrEqualTo
    case "<=" => BinaryOperatorEnum.lessThanOrEqualTo
    case "&" => BinaryOperatorEnum.and
    case "|" => BinaryOperatorEnum.or
    case "AND" => BinaryOperatorEnum.AND
    case "OR" => BinaryOperatorEnum.OR
  } | failure("binary operator mismatched")

  // literal-value
  def literalValueParser: Parser[LiteralValueExpression] = (
    ("CURRENT_TIMESTAMP" | "CURRENT_DATE" | "CURRENT_TIME" | "FALSE" | "TRUE" | "NULL")
      | numericLiteralParser
      | stringLiteralParser
      | blobLiteralParser
    )
    ^^ {
    case s: String => s match {
      case "CURRENT_TIMESTAMP" => DateTimeLiteral(LocalDate.now(), LocalTime.now())
      case "CURRENT_DATE" => DateLiteral(LocalDate.now())
      case "CURRENT_TIME" => TimeLiteral(LocalTime.now())
      case "FALSE" => BooleanLiteral(false)
      case "TRUE" => BooleanLiteral(true)
      case "NULL" => NullLiteral()
    }
    case i: IntegerLiteral => i
    case d: DoubleLiteral => d
    case s: StringLiteral => s
    case b: BlobLiteral => b
  } | failure("literal value mismatched")

  // select-stmt
  def where_clause: Parser[Selection] = "WHERE" ~> exprParser ^^ {
    case expr => Selection(expression = expr)
  } | failure("where_clause")

  def group_by_clause: Parser[GroupBy] = "GROUP" ~> "BY" ~> repsep(exprParser, ",") ~ opt("HAVING" ~> exprParser)
    ^^ {
    case expr ~ Some(e) => {
      val column_array = isColumnsExpression(expr)
      if (!column_array.isEmpty) GroupBy(groupExpressions = column_array, havingExpression = Some(e))
      else GroupBy(groupExpressions = expr.toArray, havingExpression = Some(e))
    }

    case expr ~ None => {
      val column_array = isColumnsExpression(expr)
      if (!column_array.isEmpty) GroupBy(groupExpressions = column_array, None)
      else GroupBy(groupExpressions = expr.toArray, None)
    }

  } | failure("group_by_clause")

  def window_as_defn_clause: Parser[(String, WindowDefinition)] = schemaObjectParser ~ "AS" ~ windowDefnParser ^^ {
    case window_name ~ as ~ window_defn => (window_name, window_defn)
  } | failure("window_as_defn_clause")

  def window_clause: Parser[Window] = "WINDOW" ~> repsep(window_as_defn_clause, ",") ^^ {
    case result => {
      val names = Array.empty[String]
      val defns = Array.empty[WindowDefinition]

      result.foreach{
        case (name, defn) => {
          names.appended(name)
          defns.appended(defn)
        }
      }

      Window(windowNames = names, windowDefinitions = defns)
    }
  } | failure("window_clause")

  def order_by_clause: Parser[OrderBy] = "ORDER" ~> "BY" ~> repsep(orderingItemParser, ",") ^^ {
    case ordering_item => OrderBy(ordering_item.toArray)
  } | failure("order_by_clause")

  def limit_clause: Parser[Limit] = "LIMIT" ~> exprParser ~ opt(
    ("OFFSET" ~ exprParser) | ("," ~ exprParser)
  ) ^^ {
    case expr ~ Some(result) => {
      result match {
        case "OFFSET" ~ e => Limit(limitExpression = expr, offsetExpression = Some(e))
        case "," ~ e => Limit(limitExpression = expr, offsetExpression = None)
      }
    }
    case expr ~ None => Limit(limitExpression = expr, offsetExpression = None)
  } | failure("limit_clause")

  def selectStmtParser: Parser[SelectStatement] = {"SELECT" ~> opt("DISTINCT" | "ALL")
    ~ repsep(resultColumnParser, ",")
    ~ opt(where_clause)
    ~ opt(group_by_clause)
    ~ opt(window_clause)
    ~ opt(order_by_clause)
    ~ opt(limit_clause)
  } ^^ {
    case select ~ column ~ where ~ group ~ window ~ order ~ limit => {
      select match {
        case Some(s) => s match {
          case "DISTINCT" => SelectStatement(projections = column.toArray, projectionType = Some(ProjectionTypeEnum.distinct), selection = where, groupBy = group, window = window, orderBy = order, limit = limit)
          case "ALL" => SelectStatement(projections = column.toArray, projectionType = Some(ProjectionTypeEnum.all), selection = where, groupBy = group, window = window, orderBy = order, limit = limit)
        }

        case None => SelectStatement(projections = column.toArray, projectionType = None, selection = where, groupBy = group, window = window, orderBy = order, limit = limit)
      }
    }
  } | failure("selectStmtParser")

  // expr
  def column_clause: Parser[ColumnExpression] = {
    schemaObjectParser  // column
      //||| schemaObjectParser ~ "." ~ schemaObjectParser       // table.column
      //||| schemaObjectParser ~ "." ~ schemaObjectParser ~ "." ~ schemaObjectParser //schema.table.column
      ||| schemaObjectParser ~ "/" ~ schemaObjectParser //table/column
  } ^^ {
    case result => {
      result match {
        //case schema ~ "." ~ table ~ "." ~ column => ColumnExpression(schemaName = Some(schema.toString), tableName = Some(table.toString), columnName = column.toString)
        case table ~ ("." | "/") ~ column => ColumnExpression(columnName = (table.toString,column.toString))
        case s: String => ColumnExpression(columnName = ("",s))
      }
    }
  } | failure("column_clause")

  def unary_operator_clause: Parser[UnaryOperatorExpression] = ("+"|"-") ~ exprParser ^^ {
    case operator ~ expr => operator match {
      case "+" => UnaryOperatorExpression(unaryOperator = UnaryOperatorEnum.positive, expression = expr)
      case "-" => UnaryOperatorExpression(unaryOperator = UnaryOperatorEnum.negative, expression = expr)
    }
  } | failure("unary_operator_clause")

  def distinct_clasue: Parser[String|Array[Expression]|List[Expression]] = "(" ~> opt("*"| (opt("DISTINCT") ~ repsep(exprParser, ",")) ) <~ ")"
    ^^ {
    case Some(result) => result match {
      case "*" => "*"
      case Some(d) ~ expr => expr.asInstanceOf[List[Expression]].toArray
      case None ~ expr => expr.asInstanceOf[List[Expression]]
    }

    case None => ""
  }

  def function_name_clause: Parser[Function] = functionNameParser ~ distinct_clasue ~ opt(filterClauseParser) ~ opt(overClauseParser)
    ^^ {
    case name ~ distinct ~ filter ~ over => {
      distinct match {
        // for expr with 'distinct' at the beginning
        case d: Array[Expression] => {
          ExpressionFunction(functionName = name, expressions = None, distinctExpressions = Some(d), filterClause = filter, overClause = over)
        }

        case d: List[Expression] => {
          ExpressionFunction(functionName = name, expressions = Some(d.toArray), distinctExpressions = None, filterClause = filter, overClause = over)
        }

        case d: String => {
          d match {
            case "*" => AsteriskFunction(functionName = name, filterClause = filter, overClause = over)
            case "" => EmptyFunction(functionName = name, filterClause = filter, overClause = over)
          }
        }
      }
    }
  } | failure("function_name_clause")

  lazy val binary_operator_clause: PackratParser[BinaryOperatorExpression] = exprParser ~ binaryOperatorParser ~ exprParser ^^ {
    case e_1 ~ operator ~ e_2 => BinaryOperatorExpression(leftExpression = e_1, binaryOperator = operator, rightExpression = e_2)
  }

  lazy val repeat_expr_clause: PackratParser[Expression] = "(" ~> (exprParser ||| repsep(exprParser, ",")) <~ ")" ^^ {
    case result => {
      result match {
        case r: Expression => r
        case _ => Expressions(expressions = result.asInstanceOf[List[Expression]].toArray)
      }
    }
  }

  lazy val cast_clause: PackratParser[CastExpression] = "CAST" ~> "(" ~> exprParser ~ "AS" ~ datatypeParser <~ ")"
    ^^ {
    case expr ~ as ~ type_name => CastExpression(castExpression = expr, typeName = type_name)
  }

  lazy val expr_collate_clause: PackratParser[CollateExpression] = exprParser ~ ("COLLATE" ~> schemaObjectParser) ^^ {
    case e ~ c => CollateExpression(collateExpression = e, collationName = c)
  }

  lazy val not_clause: PackratParser[Expression] = exprParser ~ opt("NOT")
    ~ ("LIKE" ~> exprParser ~ opt("ESCAPE" ~> exprParser)
    | ("GLOB"|"REGEXP"|"MATCH") ~ exprParser
    ) ^^ {
    case expr ~ not ~ result => {

      val _n = if (not != None) true else false

      result match {
        case r ~ Some(escape_statement) => {
          println(escape_statement.toString + "\t" + escape_statement.getClass)
          LikeExpression(negation = _n, expression = expr, likeExpression = r.asInstanceOf[Expression], escapeExpression = Some(escape_statement.asInstanceOf[Expression]))
        }
        case r ~ None => LikeExpression(negation = _n, expression = expr, likeExpression = r.asInstanceOf[Expression], escapeExpression = None)
        case w ~ r =>  w match {
          case "GLOB" => GlobExpression(expression = expr, negation = _n, globExpression = r.asInstanceOf[Expression])
          case "REGEXP" => RegexpExpression(expression = expr, negation = _n, regexpExpression = r.asInstanceOf[Expression])
          case "MATCH" => MatchExpression(expression = expr, negation = _n, matchExpression = r.asInstanceOf[Expression])
        }
      }
    }
  }

  lazy val expr_null_clause: PackratParser[NullExpression] = exprParser ~ ("ISNULL"|"NOTNULL"| ("NOT" ~ "NULL") ) ^^ {
    case e ~ n => n match {
      case "NOT" ~ "NULL" => NullExpression(nullType = NullTypeEnum.notNull, expression = e)
      case n: String => n match {
        case "ISNULL" => NullExpression(nullType = NullTypeEnum.isNull, expression = e)
        case "NOTNULL" => NullExpression(nullType = NullTypeEnum.notnull, expression = e)
      }
    }
  }

  lazy val is_not_clause: PackratParser[Expression] = exprParser ~ "IS" ~ opt("NOT") ~ exprParser ^^ {
    case e_1 ~ is ~ not ~ e_2 => {not match {
      case Some(n) => IsExpression(negation = false, expression = e_1, isExpression = e_2)
      case None => IsExpression(negation = true, expression = e_1, isExpression = e_2)
    }}
  }

  lazy val not_between_clause: PackratParser[Expression] = exprParser ~ opt("NOT") ~ ("BETWEEN" ~> exprParser) ~ opt("AND" ~> exprParser)
    ^^ {
    case e_1 ~ not ~ between ~ and => {
      val _n = if (not.isDefined) false else true

      and match {
        case Some(a) => BetweenExpression(expression = e_1, negation = _n, betweenExpression = between, andExpression = a)
        case None => BetweenExpression(expression = e_1, negation = _n, betweenExpression = between.asInstanceOf[BinaryOperatorExpression].leftExpression, andExpression = between.asInstanceOf[BinaryOperatorExpression].rightExpression)
      }
    }
  }

  lazy val select_or_expr: PackratParser[Option[SelectStatement|Array[Expression]]] = "(" ~> opt(selectStmtParser ||| repsep(exprParser, ",")) <~ ")" ^^ {
    case Some(result) => {
      if (result.isInstanceOf[SelectStatement]) Some(result.asInstanceOf[SelectStatement])
      else Some(result.asInstanceOf[List[Expression]].toArray)
    }
    case None => None
  }

  lazy val not_in_clause: PackratParser[Expression] = exprParser ~ opt("NOT") ~ "IN" ~ select_or_expr
    ^^ {
    case expr ~ not ~ in ~ result => {

      val _n = if (not.isDefined) false else true

      result match {
        case Some(r) => r match {
          case r: SelectStatement => InExpression(negation = _n, expression = expr, selectStatement = Some(r), expressions = None)
          case r: Array[Expression] => InExpression(negation = _n, expression = expr, selectStatement = None, expressions = Some(r))
        }
        case None => InExpression(negation = _n, expression = expr, selectStatement = None, expressions = None)
      }
    }
  }

  lazy val not_exists_clause: PackratParser[Expression] = opt(opt("NOT") ~ "EXISTS") ~ ("(" ~> selectStmtParser <~ ")")
    ^^ {
    case not_exists ~ select => {
      not_exists match {
        case Some(ne) => {
          ne match {
            case Some(n) ~ e => SelectExpression(selectType = Some(SelectTypeEnum.notExists), selectStatement = select)
            case None ~ e => SelectExpression(selectType = Some(SelectTypeEnum.exists), selectStatement = select)
          }
        }
        case None => SelectExpression(selectType = None, selectStatement = select)
      }
    }
  }

  lazy val when_clause: PackratParser[Expression] = "WHEN" ~> exprParser ^^ {
    case expr => expr
  }

  lazy val then_clause: PackratParser[Expression] = "THEN" ~> exprParser ^^ {
    case expr => expr
  }

  lazy val else_clause: PackratParser[Expression] = "ELSE" ~> exprParser ^^ {
    case expr => expr
  }

  lazy val case_clause: PackratParser[CaseExpression] = "CASE" ~> opt(exprParser) ~ rep(when_clause ~ then_clause) ~ opt(else_clause) <~ "END"
    ^^ {
    case e_1 ~ body ~ e_2 => {
      var w_clauses = Array.empty[Expression]
      var t_clauses = Array.empty[Expression]

      body.map{case w ~ t => (w, t)}.foreach((w, t) => {
        w_clauses = w_clauses :+ w
        t_clauses = t_clauses :+ t
      })

      CaseExpression(caseExpression = e_1,
        whenExpression = w_clauses,
        thenExpression = t_clauses,
        elseExpression = e_2)
    }
  }

  def expr_repeat_clause: Parser[Expressions] = "(" ~> repsep(exprParser, ",") <~ ")" ^^ {
    case expr => Expressions(expressions = expr.toArray)
  } | failure("expr_repeat_clause")

  lazy val exprParser: PackratParser[Expression] = {
    literalValueParser
      ||| binaryOperatorParser
      ||| column_clause
      ||| unary_operator_clause
      ||| binary_operator_clause
      ||| function_name_clause
      ||| repeat_expr_clause
      ||| cast_clause
      ||| expr_collate_clause
      ||| not_clause
      ||| expr_null_clause
      ||| is_not_clause
      ||| not_between_clause
      ||| not_in_clause
      ||| not_exists_clause
      ||| case_clause
      ||| raiseFunctionParser
  } ^^ {case expr => expr}

  // filter-clause
  def filterClauseParser: Parser[Selection] = "FILTER" ~> "(" ~> "WHERE" ~> exprParser <~ ")" ^^ {
    case e => Selection(expression = e)
  } | failure("filterClauseParser")

  /* over-clause
  * Ref : https://www.sqlite.org/syntax/over-clause.html
  * the bottom part should be equal to window-defn (https://www.sqlite.org/syntax/window-defn.html)
  * */
  def overClauseParser: Parser[OverClause] = "OVER" ~> (schemaObjectParser ||| windowDefnParser) ^^ {
    case s => s match {
      case s: String => OverClause(windowName = Some(s), windowDefinition = None)
      case s: WindowDefinition => OverClause(windowName = None, windowDefinition = Some(s))
    }
  } | failure("overClauseParser")

  //raise-function
  def raiseFunctionParser: Parser[RaiseFunction] = {"RAISE" ~> "("
    ~> ("IGNORE" | ("ROLLBACK"|"ABORT"|"FAIL") ~ "," ~ schemaObjectParser) <~ ")"}
    ^^ {
    case result => {
      result match {
        case f ~ comma ~ n => {
          val _f = f match {
            case "ROLLBACK" => RaiseTypeEnum.rollback
            case "ABORT" => RaiseTypeEnum.abort
            case "FAIL" => RaiseTypeEnum.fail
          }

          RaiseFunction(raiseType = _f, errorMessage = Some(n.toString))
        }
        case ignore: String => RaiseFunction(raiseType = RaiseTypeEnum.ignore, errorMessage = None)
      }
    }
  } | failure("raiseFunctionParser")

  // type-name
  def typeNameParser: Parser[TypeName] = repsep(datatypeParser, ",") ~ opt(
    ("(" ~> numericLiteralParser <~ ")")
      ||| "(" ~> numericLiteralParser ~ "," ~ numericLiteralParser <~ ")"
  )
    ^^ {
    case name ~ number => {
      (name, number) match {
        case (name, Some(n)) => {
          n match {
            case n_1: (IntegerLiteral|DoubleLiteral) => TypeName(names = name.toArray, signedNumbers = Some(Array(SignedNumber(sign = None, literal = n_1))))
            case n_1 ~ n_2 => (n_1, n_2) match {
              case (n_1: IntegerLiteral, n_2: IntegerLiteral) => TypeName(names = name.toArray, signedNumbers = Some(Array(SignedNumber(sign = None, literal = n_1.asInstanceOf[IntegerLiteral]), SignedNumber(sign = None, literal = n_2.asInstanceOf[IntegerLiteral]))))
              case (n_1: IntegerLiteral, n_2: DoubleLiteral) => TypeName(names = name.toArray, signedNumbers = Some(Array(SignedNumber(sign = None, literal = n_1.asInstanceOf[IntegerLiteral]), SignedNumber(sign = None, literal = n_2.asInstanceOf[DoubleLiteral]))))
              case (n_1: DoubleLiteral, n_2: IntegerLiteral) => TypeName(names = name.toArray, signedNumbers = Some(Array(SignedNumber(sign = None, literal = n_1.asInstanceOf[DoubleLiteral]), SignedNumber(sign = None, literal = n_2.asInstanceOf[IntegerLiteral]))))
              case (n_1: DoubleLiteral, n_2: DoubleLiteral) => TypeName(names = name.toArray, signedNumbers = Some(Array(SignedNumber(sign = None, literal = n_1.asInstanceOf[DoubleLiteral]), SignedNumber(sign = None, literal = n_2.asInstanceOf[DoubleLiteral]))))
            }
          }
        }
        case (name, None) => {TypeName(names = name.toArray, signedNumbers = None)}
      }
    }
  } | failure("typeNameParser")

  // ordering-term
  def collate_clause: PackratParser[String] = "COLLATE" ~> schemaObjectParser ^^ {
    case s => Array("COLLATE", s).mkString(" ").replaceAll(" +", " ")
  } | failure("collate_clause")

  def sort_clause: Parser[OrderEnum] = ("ASC"|"DESC") ^^ {
    case "ASC" => OrderEnum.asc
    case "DESC" => OrderEnum.desc
  } | failure("sort_clause")

  def nulls_clause: Parser[NullOrderEnum] = "NULLS" ~ ("FIRST"|"LAST") ^^ {
    case n ~ order => order match {
      case "FIRST" => NullOrderEnum.nullsFirst
      case "LAST" => NullOrderEnum.nullsLast
    }
  } | failure("nulls_clause")

  def orderingItemParser: Parser[OrderingTerm] = exprParser ~ opt("COLLATE" ~> schemaObjectParser) ~ opt(sort_clause) ~ opt(nulls_clause) ^^ {
    case expr ~ collate ~ sort ~ nulls => {
      if (expr.isInstanceOf[StringLiteral]) OrderingTerm(expression = ColumnExpression(columnName = (expr.toString.replaceAll("\"", ""), "")), collationName = collate, order = sort, nullOrder = nulls)
      else OrderingTerm(expression = expr, collationName = collate, order = sort, nullOrder = nulls)
    }
  } | failure("orderingItemParser")

  // result-column
  def resultColumnParser: Parser[Projection] = { "*"
    | (exprParser ~ opt(opt("AS") ~ schemaObjectParser))
  }
    ^^ {
    case result => {
      result match {
        case "*" => Projection(expression = None, alias = None, asterisk = Some(true))
        case expr ~ Some(content) => {
          content match {
            case Some(as) ~ n => Projection(expression = Some(expr.asInstanceOf[Expression]), alias = Some(n.toString), asterisk = None)
            case None ~ n => Projection(expression = Some(expr.asInstanceOf[Expression]), alias = Some(n.toString), asterisk = None)
          }
        }
        case expr ~ None => Projection(expression = Some(expr.asInstanceOf[Expression]), alias = None, asterisk = None)
      }
    }
  } | failure("resultColumnParser")

  // window-defn
  def partition_clause: Parser[Array[Expression]] = {"PARTITION" ~> "BY" ~> repsep(exprParser, ",")} ^^ {
    case expr => expr.toArray
  } | failure("partition_clause")

  def order_clause: Parser[Array[OrderingTerm]] = {"ORDER" ~> "BY" ~> repsep(orderingItemParser, ",")} ^^ {
    case o => o.toArray
  } | failure("order_clause")

  def windowDefnParser: Parser[WindowDefinition] = "(" ~> opt(schemaObjectParser) ~ opt(partition_clause)
    ~ opt(order_clause) ~ opt(frameSpecParser) <~ ")"
    ^^ {
    case name ~ partition ~ order ~ frame  => WindowDefinition(baseWindowName = name, partitionExpressions = partition,
      orderingTerms = order, frame = frame)
  } | failure("windowDefnParser")

  // frame-spec
  def frameSpecParser: Parser[Frame] = {
    ("RANGE"|"ROWS"|"GROUPS")
      ~ (("UNBOUNDED PRECEDING"|"CURRENT ROW")
      ||| ("BETWEEN"
      ~ ("UNBOUNDED PRECEDING" | "CURRENT ROW" | exprParser ~ ("PRECEDING"|"FOLLOWING"))
      ~ "AND" ~ ("UNBOUNDED PRECEDING"|"CURRENT ROW" | exprParser ~ ("PRECEDING"|"FOLLOWING")))
      |||  exprParser ~  "PRECEDING")
      ~ opt("EXCLUDE NO OTHERS" | "EXCLUDE CURRENT ROW" | "EXCLUDE GROUP" | "EXCLUDE TIES")
  } ^^ {
    case h ~ m ~ e => {
      val _h = h match {
        case "RANGE" => FrameTypeEnum.range
        case "ROWS" => FrameTypeEnum.rows
        case "GROUPS" => FrameTypeEnum.groups
      }

      val _e = e match {
        case Some(r) => r match {
          case "EXCLUDE NO OTHERS" => Some(ExcludeEnum.excludeNoOthers)
          case "EXCLUDE CURRENT ROW" => Some(ExcludeEnum.excludeCurrentRow)
          case "EXCLUDE GROUP" => Some(ExcludeEnum.excludeGroup)
          case "EXCLUDE TIES" => Some(ExcludeEnum.excludeTies)
        }
        case None => None
      }

      m match {
//        case between ~ core_1 ~ and ~ core_2 => {
//          val (c_1, w_1) = core_1 match {
//            case expr ~ w => w match {
//              case "preceding" => (Some(expr.asInstanceOf[Expression]), "preceding")
//              case "following" => (Some(expr.asInstanceOf[Expression]), "following")
//            }
//            case s: String => s match {
//              case "unbounded preceding" => (None, Some(FrameBoundaryEnum.unboundedPreceding))
//              case "current row" => (None, Some(FrameBoundaryEnum.currentRow))
//            }
//          }
//
//          val (c_2, w_2) = core_2 match {
//            case expr ~ w => w match {
//              case "preceding" => (Some(expr.asInstanceOf[Expression]), "preceding")
//              case "following" => (Some(expr.asInstanceOf[Expression]), "following")
//            }
//            case s: String => s match {
//              case "current row" => (None, Some(FrameBoundaryEnum.currentRow))
//              case "unbounded following" => (None, Some(FrameBoundaryEnum.unboundedFollowing))
//            }
//          }
//
//          (c_1, c_2) match {
//            case (Some(c_1), Some(c_2)) =>
//            case (Some(c_1), None) =>
//            case (None, Some(c_2)) =>
//            case (None, None) => FrameBetween(frameType = _h, betweenBoundary = w_1.asInstanceOf[Option[FrameBoundaryEnum]], betweenPrecedingExpression = None,
//              betweenFollowingExpression = None, andBoundary = w_2.asInstanceOf[Option[FrameBoundaryEnum]], andPrecedingExpression = None, andFollowingExpression = None, exclude = _e)
//          }
//        }
        case expr ~ preceding => Frame(frameType = _h, frameBoundary = None, precedingExpression = Some(expr.asInstanceOf[Expression]), excludeType = _e)
        case s: String => s match {
          case "UNBOUNDED PRECEDING" => Frame(frameType = _h, frameBoundary = Some(FrameBoundaryEnum.unboundedPreceding), precedingExpression = None, excludeType = _e)
          case "CURRENT ROW" => Frame(frameType = _h, frameBoundary = Some(FrameBoundaryEnum.currentRow), precedingExpression = None, excludeType = _e)
        }
      }
    }
  } | failure("frameSpecParser")

//  def parse(query: String): Either[String, SelectStatement] = {
//    val reader = new PackratReader[Char](new CharSequenceReader(query))
//
//    parse(selectStmtParser, reader) match {
//      case Success(matched, _) => Right(matched)
//      case Failure(msg, _)     => Left(msg)
//      case Error(msg, _)       => throw new Exception(s"Parser error: ${msg}")
//    }
//  }

  def parse(s: String): Either[String, SelectStatement] = phrase(selectStmtParser)(new PackratReader(new lexical.Scanner(s))) match {
    case Success(matched, _) => Right(matched)
    case Failure(s, _)       => Left(s)
    case Error(s, _)         => throw new Exception(s"Parser error: ${s}")
  }
}

object Parser {
  def apply(query: String) = new Parser().parse(query)
}
