
//***************************************************************************************
//
// Title:   SQL Parser Test Suite
// Author:  Xiaomin Wang, Alex Schuler, Shue Kwan Ip, Ahmet Kösker
// Date:    January 25th, 2022
//
//***************************************************************************************

import org.scalatest.funsuite.*
import org.scalatest.matchers.should.*
import ir.*

class SQLParserTest extends AnyFunSuite with Matchers{
  //SQL 0
  test("sql_0") {
    val result: Either[String, SelectStatement] = Parser.apply(sql_0)
    //print(result.toString())
    result.getOrElse(null) shouldBe a [SelectStatement]
    val smt: SelectStatement = result.getOrElse(null).asInstanceOf[SelectStatement]

    //SELECT car/color
    smt.projections(0).expression.getOrElse(null) shouldBe a [ColumnExpression]
    val colExpr1: ColumnExpression = smt.projections(0).expression.getOrElse(null).asInstanceOf[ColumnExpression]
    colExpr1.columnName._1 should === ("""car""")
    colExpr1.columnName._2 should === ("""color""")

    //WHERE car/make="Ford"
    smt.selection.getOrElse(null).expression shouldBe a [BinaryOperatorExpression]
    val binOperExpr: BinaryOperatorExpression = smt.selection.getOrElse(null).expression.asInstanceOf[BinaryOperatorExpression]
    binOperExpr.leftExpression shouldBe a [ColumnExpression]
    val colExpr2: ColumnExpression = binOperExpr.leftExpression.asInstanceOf[ColumnExpression]
    colExpr2.columnName._1 should === ("""car""")
    colExpr2.columnName._2 should === ( """make""")

    binOperExpr.binaryOperator should === (BinaryOperatorEnum.equalTo)

    binOperExpr.rightExpression shouldBe a [LiteralValueExpression]
    val literValExpr1: LiteralValueExpression = binOperExpr.rightExpression.asInstanceOf[LiteralValueExpression]
    literValExpr1 shouldBe a [StringLiteral]
    val strLiter: StringLiteral = literValExpr1.asInstanceOf[StringLiteral]
    strLiter.value should === ("""Ford""")

    //LIMIT 3
    smt.limit.get.limitExpression shouldBe a [LiteralValueExpression]
    val literValExpr2: LiteralValueExpression = smt.limit.get.limitExpression.asInstanceOf[LiteralValueExpression]
    literValExpr2 shouldBe a [IntegerLiteral]
    val intLiter: IntegerLiteral = literValExpr2.asInstanceOf[IntegerLiteral]
    intLiter.value should === (3)

  }

  //SQL 1
  test("sql_1") {
    val result: Either[String, SelectStatement] = Parser.apply(sql_1)
    result.getOrElse(null) shouldBe a [SelectStatement]
    val smt: SelectStatement = result.getOrElse(null).asInstanceOf[SelectStatement]
    //print(result.toString())
    //ALL
    val proType = smt.projectionType.get
    proType shouldBe ProjectionTypeEnum.all
    //lineitem/shipmode
    smt.projections(0).expression.getOrElse(null) shouldBe a [ColumnExpression]
    val colExpr: ColumnExpression = smt.projections(0).expression.getOrElse(null).asInstanceOf[ColumnExpression]
    colExpr.columnName._1 should === ("""lineitem""")
    colExpr.columnName._2 should === ("""shipmode""")
  }

  //SQL 2
  test("sql_2") {
    val result: Either[String, SelectStatement] = Parser.apply(sql_2)
    //print(result.toString())
    result.getOrElse(null) shouldBe a [SelectStatement]
    val smt: SelectStatement = result.getOrElse(null).asInstanceOf[SelectStatement]

    //DISTINCT
    val proType = smt.projectionType.get
    proType shouldBe ProjectionTypeEnum.distinct
    //lineitem/shipmode
    smt.projections(0).expression.getOrElse(null) shouldBe a [ColumnExpression]
    val colExpr: ColumnExpression = smt.projections(0).expression.getOrElse(null).asInstanceOf[ColumnExpression]
    colExpr.columnName._1 should === ("""lineitem""")
    colExpr.columnName._2 should === ("""shipmode""")
  }

  //SQL 3
  test("sql_3") {
    val result: Either[String, SelectStatement] = Parser.apply(sql_3)
    //print(result.toString())
    result.getOrElse(null) shouldBe a [SelectStatement]
    val smt: SelectStatement = result.getOrElse(null).asInstanceOf[SelectStatement]
    //lineitem/shipmode Shipmode
    smt.projections(0).expression.getOrElse(null) shouldBe a [ColumnExpression]
    val colExpr: ColumnExpression = smt.projections(0).expression.getOrElse(null).asInstanceOf[ColumnExpression]
    colExpr.columnName._1 should === ("""lineitem""")
    colExpr.columnName._2 should ===("""shipmode""")

    smt.projections(0).alias.getOrElse("") should ===("""Shipmode""")
    //lineitem/name AS Name
    smt.projections(1).expression.getOrElse(null) shouldBe a [ColumnExpression]
    val colExpr1: ColumnExpression = smt.projections(1).expression.getOrElse(null).asInstanceOf[ColumnExpression]
    colExpr1.columnName._1 should === ("""lineitem""")
    colExpr1.columnName._2 should === ("""name""")

    smt.projections(1).alias.getOrElse("") should === ("""Name""")
  }

  //SQL 4
  test("sql_4") {
    val result: Either[String, SelectStatement] = Parser.apply(sql_4)
    //print(result.toString())
    result.getOrElse(null) shouldBe a [SelectStatement]
    val smt: SelectStatement = result.getOrElse(null).asInstanceOf[SelectStatement]
    // *
    smt.projections(0).asterisk.getOrElse(false) shouldBe true

  }

  //SQL 5
  test("sql_5") {
    val result: Either[String, SelectStatement] = Parser.apply(sql_5)
    //print(result.toString())
    result.getOrElse(null) shouldBe a [SelectStatement]
    val smt: SelectStatement = result.getOrElse(null).asInstanceOf[SelectStatement]

    // CAST(25.65 AS INT)
    smt.projections(0).expression.getOrElse(null) shouldBe a [CastExpression]
    val castExpr: CastExpression = smt.projections(0).expression.getOrElse(null).asInstanceOf[CastExpression]
    castExpr.castExpression shouldBe a [LiteralValueExpression]
    val literValExpr: LiteralValueExpression = castExpr.castExpression.asInstanceOf[LiteralValueExpression]
    literValExpr shouldBe a [DoubleLiteral]
    val doubleLiter: DoubleLiteral = literValExpr.asInstanceOf[DoubleLiteral]
    doubleLiter.value should ===(25.65)
    castExpr.typeName should === ("""INT""")
  }

  //SQL 6
  test("sql_6") {
    val result: Either[String, SelectStatement] = Parser.apply(sql_6)
    //print(result.toString())
    result.getOrElse(null) shouldBe a [SelectStatement]
    val smt: SelectStatement = result.getOrElse(null).asInstanceOf[SelectStatement]

    //part/partkey
    smt.projections(0).expression.getOrElse(null) shouldBe a [ColumnExpression]
    val colExpr: ColumnExpression = smt.projections(0).expression.getOrElse(null).asInstanceOf[ColumnExpression]
    colExpr.columnName._1 should === ("""part""")
    colExpr.columnName._2 should === ( """partkey""")

    //part/partkey = "key"
    smt.selection.get.expression shouldBe a [BinaryOperatorExpression]
    val binOperExpr: BinaryOperatorExpression = smt.selection.get.expression.asInstanceOf[BinaryOperatorExpression]
    binOperExpr.leftExpression shouldBe a [ColumnExpression]
    val colExpr1: ColumnExpression = binOperExpr.leftExpression.asInstanceOf[ColumnExpression]
    colExpr1.columnName._1 should === ("""part""")
    colExpr1.columnName._2 should === ( """partkey""")

    binOperExpr.binaryOperator shouldBe  BinaryOperatorEnum.equalTo

    binOperExpr.rightExpression shouldBe a [LiteralValueExpression]
    val literValExpr: LiteralValueExpression = binOperExpr.rightExpression.asInstanceOf[LiteralValueExpression]
    literValExpr shouldBe a [StringLiteral]
    val strLiter: StringLiteral = literValExpr.asInstanceOf[StringLiteral]
    strLiter.value should === ("""key""")
  }

  //SQL 7
  test("sql_7") {
    val result: Either[String, SelectStatement] = Parser.apply(sql_7)
    //print(result.toString())
    result.getOrElse(null) shouldBe a [SelectStatement]
    val smt: SelectStatement = result.getOrElse(null).asInstanceOf[SelectStatement]
    //lineitem/quantity
    smt.projections(0).expression.getOrElse(null) shouldBe a [ColumnExpression]
    val colExpr: ColumnExpression = smt.projections(0).expression.getOrElse(null).asInstanceOf[ColumnExpression]
    colExpr.columnName._1 should === ("""lineitem""")
    colExpr.columnName._2 should ===( """quantity""")
    //-lineitem/quantity != -3
    smt.selection.get.expression shouldBe a [UnaryOperatorExpression]
    val unOperExpr: UnaryOperatorExpression = smt.selection.get.expression.asInstanceOf[UnaryOperatorExpression]
    unOperExpr.unaryOperator shouldBe UnaryOperatorEnum.negative
    unOperExpr.expression shouldBe a [BinaryOperatorExpression]
    val binOperExpr: BinaryOperatorExpression = unOperExpr.expression.asInstanceOf[BinaryOperatorExpression]
    binOperExpr.leftExpression shouldBe a [ColumnExpression]
    val colExpr1: ColumnExpression = binOperExpr.leftExpression.asInstanceOf[ColumnExpression]
    colExpr1.columnName._1 should === ("""lineitem""")
    colExpr1.columnName._2 should === ( """quantity""")

    binOperExpr.binaryOperator shouldBe BinaryOperatorEnum.notEqualTo

    binOperExpr.rightExpression shouldBe a [UnaryOperatorExpression]
    val unOperExpr1: UnaryOperatorExpression = binOperExpr.rightExpression.asInstanceOf[UnaryOperatorExpression]
    unOperExpr1.unaryOperator shouldBe UnaryOperatorEnum.negative
    unOperExpr1.expression shouldBe a [LiteralValueExpression]
    val literValExpr: LiteralValueExpression = unOperExpr1.expression.asInstanceOf[LiteralValueExpression]
    literValExpr shouldBe a [IntegerLiteral]
    val intLiter: IntegerLiteral = literValExpr.asInstanceOf[IntegerLiteral]
    intLiter.value should === (3)
  }

  //SQL 8
  test("sql_8") {
    val result: Either[String, SelectStatement] = Parser.apply(sql_8)
    //print(result.toString())
    result.getOrElse(null) shouldBe a [SelectStatement]
    val smt: SelectStatement = result.getOrElse(null).asInstanceOf[SelectStatement]
    //customer/name
    smt.projections(0).expression.getOrElse(null).isInstanceOf[ColumnExpression]
    val colExpr: ColumnExpression = smt.projections(0).expression.getOrElse(null).asInstanceOf[ColumnExpression]
    colExpr.columnName._1 should === ("""customer""")
    colExpr.columnName._2 should === ( """name""")
    //customer/name COLLATE SQL_General_CP1_CS_AS
    smt.selection.get.expression shouldBe a [CollateExpression]
    val collExpr:CollateExpression = smt.selection.get.expression.asInstanceOf[CollateExpression]
    collExpr.collateExpression shouldBe a [ColumnExpression]
    val colExpr1: ColumnExpression = collExpr.collateExpression.asInstanceOf[ColumnExpression]
    //print(collExpr)
    colExpr1.columnName._1 should === ("""customer""")
    colExpr1.columnName._2 should === ( """name""")
    collExpr.collationName should === ("""SQL_General_CP1_CS_AS""")

  }

  //SQL 9 escape
  test("sql_9") {
    val result: Either[String, SelectStatement] = Parser.apply(sql_9)
    //print(result.toString())
    result.getOrElse(null) shouldBe a [SelectStatement]
    val smt: SelectStatement = result.getOrElse(null).asInstanceOf[SelectStatement]
    //part/type
    smt.projections(0).expression.getOrElse(null).isInstanceOf[ColumnExpression]
    val colExpr: ColumnExpression = smt.projections(0).expression.getOrElse(null).asInstanceOf[ColumnExpression]
    colExpr.columnName._1 should === ("""part""")
    colExpr.columnName._2 should ===( """type""")
    //customer/name
    smt.projections(1).expression.getOrElse(null).isInstanceOf[ColumnExpression]
    val colExpr1: ColumnExpression = smt.projections(1).expression.getOrElse(null).asInstanceOf[ColumnExpression]
    colExpr1.columnName._1 should === ("""customer""")
    colExpr1.columnName._2 should === ( """name""")
    //supplier/name
    smt.projections(2).expression.getOrElse(null).isInstanceOf[ColumnExpression]
    val colExpr2: ColumnExpression = smt.projections(2).expression.getOrElse(null).asInstanceOf[ColumnExpression]
    colExpr2.columnName._1 should === ("""supplier""")
    colExpr2.columnName._2 should === ( """name""")
    //region/name
    smt.projections(3).expression.getOrElse(null).isInstanceOf[ColumnExpression]
    val colExpr3: ColumnExpression = smt.projections(3).expression.getOrElse(null).asInstanceOf[ColumnExpression]
    colExpr3.columnName._1 should === ("""region""")
    colExpr3.columnName._2 should === ( """name""")

    smt.selection.get.expression shouldBe a [BinaryOperatorExpression]
    val binOperExpr: BinaryOperatorExpression = smt.selection.get.expression.asInstanceOf[BinaryOperatorExpression]
    //part/type LIKE "%foo" ESCAPE "\"
    binOperExpr.leftExpression shouldBe a [LikeExpression]
    val likeExpr: LikeExpression = binOperExpr.leftExpression.asInstanceOf[LikeExpression]
    likeExpr.expression shouldBe a [ColumnExpression]
    val colExpr4: ColumnExpression = likeExpr.expression.asInstanceOf[ColumnExpression]
    colExpr4.columnName._1 should === ("""part""")
    colExpr4.columnName._2 should === ( """type""")

    likeExpr.likeExpression shouldBe a [LiteralValueExpression]
    val literValExpr: LiteralValueExpression = likeExpr.likeExpression.asInstanceOf[LiteralValueExpression]
    literValExpr shouldBe a [StringLiteral]
    val strLiter: StringLiteral = literValExpr.asInstanceOf[StringLiteral]
    strLiter.value should === ("""%foo""")

    likeExpr.escapeExpression.getOrElse(null) shouldBe a [LiteralValueExpression]
    val literValExpr1: LiteralValueExpression = likeExpr.escapeExpression.getOrElse(null).asInstanceOf[LiteralValueExpression]
    literValExpr1 shouldBe a [StringLiteral]
    val strLiter1: StringLiteral = literValExpr1.asInstanceOf[StringLiteral]
    strLiter1.value should === ("""\""")

    likeExpr.negation shouldBe  false
    //AND customer/name NOT GLOB 'Man*'
    binOperExpr.binaryOperator shouldBe BinaryOperatorEnum.AND
    binOperExpr.rightExpression shouldBe a [BinaryOperatorExpression]
    val binOperExpr1:BinaryOperatorExpression = binOperExpr.rightExpression.asInstanceOf[BinaryOperatorExpression]
    binOperExpr1.leftExpression shouldBe a [GlobExpression]
    val globExpr:GlobExpression = binOperExpr1.leftExpression.asInstanceOf[GlobExpression]
    globExpr.expression shouldBe a [ColumnExpression]
    val colExpr5: ColumnExpression = globExpr.expression.asInstanceOf[ColumnExpression]
    colExpr5.columnName._1 should === ("""customer""")
    colExpr5.columnName._2 should === ( """name""")

    globExpr.negation shouldBe true
    globExpr.globExpression shouldBe a [LiteralValueExpression]
    val literValExpr2: LiteralValueExpression = globExpr.globExpression.asInstanceOf[LiteralValueExpression]
    literValExpr2 shouldBe a [StringLiteral]
    val strLiter2: StringLiteral = literValExpr2.asInstanceOf[StringLiteral]
    strLiter2.value should === ("""Man*""")

    //OR supplier/name REGEXP '^[abcd]'
    binOperExpr1.binaryOperator shouldBe BinaryOperatorEnum.OR
    binOperExpr1.rightExpression shouldBe a [BinaryOperatorExpression]
    val binOperExpr2:BinaryOperatorExpression = binOperExpr1.rightExpression.asInstanceOf[BinaryOperatorExpression]
    binOperExpr2.leftExpression shouldBe a [RegexpExpression]
    val regexExpr:RegexpExpression = binOperExpr2.leftExpression.asInstanceOf[RegexpExpression]
    regexExpr.expression shouldBe a [ColumnExpression]
    val colExpr6: ColumnExpression = regexExpr.expression.asInstanceOf[ColumnExpression]
    colExpr6.columnName._1.equalsIgnoreCase("""supplier""")
    colExpr6.columnName._2.equalsIgnoreCase( """name""")

    regexExpr.negation shouldBe false
    regexExpr.regexpExpression shouldBe a [LiteralValueExpression]
    val literValExpr3: LiteralValueExpression = regexExpr.regexpExpression.asInstanceOf[LiteralValueExpression]
    literValExpr3 shouldBe a [StringLiteral]
    val strLiter3: StringLiteral = literValExpr3.asInstanceOf[StringLiteral]
    strLiter3.value should === ("""^[abcd]""")

    //AND region/name MATCH 'R?x[a-z]'
    binOperExpr2.binaryOperator shouldBe BinaryOperatorEnum.AND
    binOperExpr2.rightExpression shouldBe a [MatchExpression]
    val matchExpr:MatchExpression = binOperExpr2.rightExpression.asInstanceOf[MatchExpression]

    matchExpr.expression shouldBe a [ColumnExpression]
    val colExpr7: ColumnExpression = matchExpr.expression.asInstanceOf[ColumnExpression]
    colExpr7.columnName._1 should === ("""region""")
    colExpr7.columnName._2 should === ( """name""")
    matchExpr.negation shouldBe false
    matchExpr.matchExpression shouldBe a [LiteralValueExpression]
    val literValExpr4: LiteralValueExpression = matchExpr.matchExpression.asInstanceOf[LiteralValueExpression]
    literValExpr4 shouldBe a [StringLiteral]
    val strLiter4: StringLiteral = literValExpr4.asInstanceOf[StringLiteral]
    strLiter4.value === ("""^[abcd]""")
}


  //SQL 10
  test("sql_10") {
    val result: Either[String, SelectStatement] = Parser.apply(sql_10)
    //print(result.toString())
    result.getOrElse(null) shouldBe a [SelectStatement]
    val smt: SelectStatement = result.getOrElse(null).asInstanceOf[SelectStatement]
    //customer/acctbal
    smt.projections(0).expression.getOrElse(null) shouldBe a [ColumnExpression]
    val colExpr: ColumnExpression = smt.projections(0).expression.getOrElse(null).asInstanceOf[ColumnExpression]
    colExpr.columnName._1 should === ("""customer""")
    colExpr.columnName._2 should === ( """acctbal""")
    //customer/name
    smt.projections(1).expression.getOrElse(null) shouldBe a [ColumnExpression]
    val colExpr1: ColumnExpression = smt.projections(1).expression.getOrElse(null).asInstanceOf[ColumnExpression]
    colExpr1.columnName._1 should === ("""customer""")
    colExpr1.columnName._2 should === ( """name""")
    //customer/address
    smt.projections(2).expression.getOrElse(null) shouldBe a [ColumnExpression]
    val colExpr2: ColumnExpression = smt.projections(2).expression.getOrElse(null).asInstanceOf[ColumnExpression]
    colExpr2.columnName._1 should === ("""customer""")
    colExpr2.columnName._2 should === ( """address""")
    smt.selection.get.expression shouldBe a [BinaryOperatorExpression]
    val binOperExpr: BinaryOperatorExpression = smt.selection.get.expression.asInstanceOf[BinaryOperatorExpression]
    //customer/acctbal ISNULL
    binOperExpr.leftExpression shouldBe a [NullExpression]
    val nullExpr:NullExpression = binOperExpr.leftExpression.asInstanceOf[NullExpression]
    nullExpr.expression shouldBe a [ColumnExpression]
    val colExpr3: ColumnExpression = nullExpr.expression.asInstanceOf[ColumnExpression]
    colExpr3.columnName._1 should === ("""customer""")
    colExpr3.columnName._2 should === ( """acctbal""")
    nullExpr.nullType shouldBe NullTypeEnum.isNull
    //AND customer/name NOTNULL
    binOperExpr.binaryOperator shouldBe BinaryOperatorEnum.AND
    binOperExpr.rightExpression shouldBe a [BinaryOperatorExpression]
    val binOperExpr1: BinaryOperatorExpression = binOperExpr.rightExpression.asInstanceOf[BinaryOperatorExpression]
    binOperExpr1.leftExpression shouldBe a [NullExpression]
    val nullExpr1:NullExpression = binOperExpr1.leftExpression.asInstanceOf[NullExpression]
    nullExpr1.expression shouldBe a [ColumnExpression]
    val colExpr4: ColumnExpression = nullExpr1.expression.asInstanceOf[ColumnExpression]
    colExpr4.columnName._1 should === ("""customer""")
    colExpr4.columnName._2 should === ( """name""")
    nullExpr1.nullType shouldBe NullTypeEnum.notnull

    //AND customer/address NOT NULL
    binOperExpr.binaryOperator shouldBe BinaryOperatorEnum.AND
    binOperExpr1.rightExpression shouldBe a [NullExpression]
    val nullExpr2:NullExpression = binOperExpr1.rightExpression.asInstanceOf[NullExpression]
    nullExpr2.expression shouldBe a [ColumnExpression]
    val colExpr5: ColumnExpression = nullExpr2.expression.asInstanceOf[ColumnExpression]
    colExpr5.columnName._1 should === ("""customer""")
    colExpr5.columnName._2 should === ( """address""")
    nullExpr2.nullType shouldBe NullTypeEnum.notNull

  }

  //SQL 11
  test("sql_11") {
    val result: Either[String, SelectStatement] = Parser.apply(sql_11)
    //print(result.toString())
    result.getOrElse(null) shouldBe a [SelectStatement]
    val smt: SelectStatement = result.getOrElse(null).asInstanceOf[SelectStatement]
    //customer/acctbal
    smt.projections(0).expression.getOrElse(null) shouldBe a [ColumnExpression]
    val colExpr: ColumnExpression = smt.projections(0).expression.getOrElse(null).asInstanceOf[ColumnExpression]
    colExpr.columnName._1 should === ("""customer""")
    colExpr.columnName._2 should === ( """acctbal""")
    //customer/name
    smt.projections(1).expression.getOrElse(null) shouldBe a [ColumnExpression]
    val colExpr1: ColumnExpression = smt.projections(1).expression.getOrElse(null).asInstanceOf[ColumnExpression]
    colExpr1.columnName._1 should === ("""customer""")
    colExpr1.columnName._2 should === ( """name""")

    smt.selection.get.expression shouldBe a [BinaryOperatorExpression]
    val binOperExpr: BinaryOperatorExpression = smt.selection.get.expression.asInstanceOf[BinaryOperatorExpression]
    //customer/acctbal IS NULL
    binOperExpr.leftExpression shouldBe a [IsExpression]
    val isExpr:IsExpression = binOperExpr.leftExpression.asInstanceOf[IsExpression]
    isExpr.expression shouldBe a [ColumnExpression]
    val colExpr2: ColumnExpression = isExpr.expression.asInstanceOf[ColumnExpression]
    colExpr2.columnName._1 should === ("""customer""")
    colExpr2.columnName._2 should === ( """acctbal""")
    isExpr.negation shouldBe true
    //to do isExpr.expression

    //AND customer/name IS NOT NULL
    binOperExpr.binaryOperator shouldBe BinaryOperatorEnum.AND
    binOperExpr.rightExpression shouldBe a [IsExpression]
    val isExpr1:IsExpression = binOperExpr.rightExpression.asInstanceOf[IsExpression]
    isExpr1.expression shouldBe a [ColumnExpression]
    val colExpr3: ColumnExpression = isExpr1.expression.asInstanceOf[ColumnExpression]
    colExpr3.columnName._1 should === ("""customer""")
    colExpr3.columnName._2 should === ( """name""")
    isExpr1.negation shouldBe false
  }

  //SQL 12
  test("sql_12") {
    val result: Either[String, SelectStatement] = Parser.apply(sql_12)
    //print(result.toString())
    result.getOrElse(null) shouldBe a [SelectStatement]
    val smt: SelectStatement = result.getOrElse(null).asInstanceOf[SelectStatement]
    //product/orderdate
    smt.projections(0).expression.getOrElse(null) shouldBe a [ColumnExpression]
    val colExpr: ColumnExpression = smt.projections(0).expression.getOrElse(null).asInstanceOf[ColumnExpression]
    colExpr.columnName._1 should === ("""product""")
    colExpr.columnName._2 should === ( """orderdate""")
    //lineitem/discount
    smt.projections(1).expression.getOrElse(null).isInstanceOf[ColumnExpression]
    val colExpr1: ColumnExpression = smt.projections(1).expression.getOrElse(null).asInstanceOf[ColumnExpression]
    colExpr1.columnName._1 should === ("""lineitem""")
    colExpr1.columnName._2 should === ( """discount""")

    smt.selection.get.expression shouldBe a [BinaryOperatorExpression]
    val binOperExpr: BinaryOperatorExpression = smt.selection.get.expression.asInstanceOf[BinaryOperatorExpression]
    //product/orderdate BETWEEN '1995-01-01' AND '1996-12-31'
    binOperExpr.leftExpression shouldBe a [BetweenExpression]
    val betweenExpr:BetweenExpression = binOperExpr.leftExpression.asInstanceOf[BetweenExpression]
    betweenExpr.negation shouldBe true
    betweenExpr.expression shouldBe a [ColumnExpression]
    val colExpr2: ColumnExpression = betweenExpr.expression.asInstanceOf[ColumnExpression]
    colExpr2.columnName._1 should === ("""product""")
    colExpr2.columnName._2 should === ( """orderdate""")

    betweenExpr.betweenExpression shouldBe a [LiteralValueExpression]
    val literValExpr: LiteralValueExpression = betweenExpr.betweenExpression.asInstanceOf[LiteralValueExpression]
    literValExpr shouldBe a [StringLiteral]
    val strLiter: StringLiteral = literValExpr.asInstanceOf[StringLiteral]
    strLiter.value should === ("""1995-01-01""")

    betweenExpr.andExpression shouldBe a [LiteralValueExpression]
    val literValExpr1: LiteralValueExpression = betweenExpr.andExpression.asInstanceOf[LiteralValueExpression]
    literValExpr1 shouldBe a [StringLiteral]
    val strLiter1: StringLiteral = literValExpr1.asInstanceOf[StringLiteral]
    strLiter1.value should === ("""1996-12-31""")

    //OR lineitem/discount NOT BETWEEN 2 - 0.01 AND 2 + 0.01
    binOperExpr.binaryOperator shouldBe BinaryOperatorEnum.OR
    binOperExpr.rightExpression shouldBe a [BetweenExpression]
    val betweenExpr1:BetweenExpression = binOperExpr.rightExpression.asInstanceOf[BetweenExpression]
    betweenExpr1.negation shouldBe false
    betweenExpr1.betweenExpression shouldBe a [BinaryOperatorExpression]
    val binOperExpr1:BinaryOperatorExpression = betweenExpr1.betweenExpression.asInstanceOf[BinaryOperatorExpression]
    binOperExpr1.leftExpression shouldBe a [LiteralValueExpression]
    val literValExpr2: LiteralValueExpression = binOperExpr1.leftExpression.asInstanceOf[LiteralValueExpression]
    literValExpr2.isInstanceOf[IntegerLiteral]
    val intLiter: IntegerLiteral = literValExpr2.asInstanceOf[IntegerLiteral]
    intLiter.value should === (2)

    binOperExpr1.binaryOperator shouldBe BinaryOperatorEnum.subtract
    binOperExpr1.rightExpression shouldBe a [LiteralValueExpression]
    val literValExpr3: LiteralValueExpression = binOperExpr1.rightExpression.asInstanceOf[LiteralValueExpression]
    literValExpr3 shouldBe a [DoubleLiteral]
    val doubleLiter: DoubleLiteral = literValExpr3.asInstanceOf[DoubleLiteral]
    doubleLiter.value should === (0.01)

    betweenExpr1.andExpression shouldBe a [BinaryOperatorExpression]
    val binOperExpr2:BinaryOperatorExpression = betweenExpr1.andExpression.asInstanceOf[BinaryOperatorExpression]
    binOperExpr2.leftExpression shouldBe a [LiteralValueExpression]
    val literValExpr4: LiteralValueExpression = binOperExpr2.leftExpression.asInstanceOf[LiteralValueExpression]
    literValExpr4.isInstanceOf[IntegerLiteral]
    val intLiter1: IntegerLiteral = literValExpr4.asInstanceOf[IntegerLiteral]
    intLiter1.value should === (2)

    binOperExpr2.binaryOperator shouldBe BinaryOperatorEnum.add
    binOperExpr2.rightExpression shouldBe a [LiteralValueExpression]
    val literValExpr5: LiteralValueExpression = binOperExpr2.rightExpression.asInstanceOf[LiteralValueExpression]
    literValExpr5 shouldBe a [DoubleLiteral]
    val doubleLiter1: DoubleLiteral = literValExpr5.asInstanceOf[DoubleLiteral]
    doubleLiter1.value should === (0.01)

  }

  //SQL 13
  test("sql_13") {
    val result: Either[String, SelectStatement] = Parser.apply(sql_13)
    //print(result.toString())
    result.right.getOrElse(null) shouldBe a [SelectStatement]
    val smt: SelectStatement = result.right.getOrElse(null).asInstanceOf[SelectStatement]
    //lineitem/shipmode
    smt.projections(0).expression.getOrElse(null) shouldBe a [ColumnExpression]
    val colExpr: ColumnExpression = smt.projections(0).expression.getOrElse(null).asInstanceOf[ColumnExpression]
    colExpr.columnName._1 should === ("""lineitem""")
    colExpr.columnName._2 should === ( """shipmode""")
    //part/size
    smt.projections(1).expression.getOrElse(null) shouldBe a [ColumnExpression]
    val colExpr1: ColumnExpression = smt.projections(1).expression.getOrElse(null).asInstanceOf[ColumnExpression]
    colExpr1.columnName._1 should === ("""part""")
    colExpr1.columnName._2 should === ( """size""")
    //product/orderkey
    smt.projections(2).expression.getOrElse(null) shouldBe a [ColumnExpression]
    val colExpr2: ColumnExpression = smt.projections(2).expression.getOrElse(null).asInstanceOf[ColumnExpression]
    colExpr2.columnName._1 should === ("""product""")
    colExpr2.columnName._2 should === ( """orderkey""")

    smt.selection.get.expression shouldBe a [BinaryOperatorExpression]
    val binOperExpr: BinaryOperatorExpression = smt.selection.get.expression.asInstanceOf[BinaryOperatorExpression]

    binOperExpr.leftExpression shouldBe a [BinaryOperatorExpression]
    val binOperExpr1:BinaryOperatorExpression = binOperExpr.leftExpression.asInstanceOf[BinaryOperatorExpression]
    //lineitem/shipmode IN ('mode0', 'mode1')
    binOperExpr1.leftExpression shouldBe a [InExpression]
    val inExpr:InExpression = binOperExpr1.leftExpression.asInstanceOf[InExpression]
    inExpr.negation shouldBe true
    inExpr.expression shouldBe a [ColumnExpression]
    val colExpr3: ColumnExpression = inExpr.expression.asInstanceOf[ColumnExpression]
    colExpr3.columnName._1 should === ("""lineitem""")
    colExpr3.columnName._2 should === ( """shipmode""")

    val exprs:Array[Expression] = inExpr.expressions.getOrElse(null)
    exprs(0) shouldBe a [LiteralValueExpression]
    val literValExpr: LiteralValueExpression = exprs(0).asInstanceOf[LiteralValueExpression]
    literValExpr shouldBe a [StringLiteral]
    val strLiter: StringLiteral = literValExpr.asInstanceOf[StringLiteral]
    strLiter.value should === ("""mode0""")

    exprs(1) shouldBe a [LiteralValueExpression]
    val literValExpr1: LiteralValueExpression = exprs(1).asInstanceOf[LiteralValueExpression]
    literValExpr1 shouldBe a [StringLiteral]
    val strLiter1: StringLiteral = literValExpr1.asInstanceOf[StringLiteral]
    strLiter1.value should === ("""mode1""")

    //AND part/size NOT IN (3, 4, 5, 6)
    binOperExpr1.binaryOperator shouldBe BinaryOperatorEnum.AND
    binOperExpr1.rightExpression shouldBe a [InExpression]
    val inExpr1:InExpression = binOperExpr1.rightExpression.asInstanceOf[InExpression]
    inExpr1.negation shouldBe false
    inExpr1.expression shouldBe a [ColumnExpression]
    val colExpr4: ColumnExpression = inExpr1.expression.asInstanceOf[ColumnExpression]
    colExpr4.columnName._1 should === ("""part""")
    colExpr4.columnName._2 should === ( """size""")

    val exprs1:Array[Expression] = inExpr1.expressions.getOrElse(null)
    exprs1(0) shouldBe a [LiteralValueExpression]
    val literValExpr2: LiteralValueExpression = exprs1(0).asInstanceOf[LiteralValueExpression]
    literValExpr2 shouldBe a [IntegerLiteral]
    val intLiter: IntegerLiteral = literValExpr2.asInstanceOf[IntegerLiteral]
    intLiter.value should === (3)

    exprs1(1) shouldBe a [LiteralValueExpression]
    val literValExpr3: LiteralValueExpression = exprs1(1).asInstanceOf[LiteralValueExpression]
    literValExpr3 shouldBe a [IntegerLiteral]
    val intLiter1: IntegerLiteral = literValExpr3.asInstanceOf[IntegerLiteral]
    intLiter1.value should === (4)
    exprs1(2) shouldBe a [LiteralValueExpression]
    val literValExpr4: LiteralValueExpression = exprs1(2).asInstanceOf[LiteralValueExpression]
    literValExpr4 shouldBe a [IntegerLiteral]
    val intLiter2: IntegerLiteral = literValExpr4.asInstanceOf[IntegerLiteral]
    intLiter2.value should === (5)

    exprs1(3) shouldBe a [LiteralValueExpression]
    val literValExpr5: LiteralValueExpression = exprs1(3).asInstanceOf[LiteralValueExpression]
    literValExpr5 shouldBe a [IntegerLiteral]
    val intLiter3: IntegerLiteral = literValExpr5.asInstanceOf[IntegerLiteral]
    intLiter3.value should === (6)

    binOperExpr.binaryOperator shouldBe BinaryOperatorEnum.OR
    binOperExpr.rightExpression shouldBe a [BinaryOperatorExpression]
    val binOperExpr2:BinaryOperatorExpression = binOperExpr.rightExpression.asInstanceOf[BinaryOperatorExpression]
    //    product/orderkey IN (
    //        SELECT
    //            lineitem/orderkey
    //        WHERE
    //            lineitem/orderkey = key
    //        )
    binOperExpr2.leftExpression shouldBe a [InExpression]
    val inExpr2:InExpression = binOperExpr2.leftExpression.asInstanceOf[InExpression]
    inExpr2.negation shouldBe true
    inExpr2.expression shouldBe a [ColumnExpression]
    val colExpr5: ColumnExpression = inExpr2.expression.asInstanceOf[ColumnExpression]
    colExpr5.columnName._1 should === ("""product""")
    colExpr5.columnName._2 should === ( """orderkey""")

    val selectSmt:SelectStatement = inExpr2.selectStatement.getOrElse(null)
    selectSmt.projections(0).expression.getOrElse(null).isInstanceOf[ColumnExpression]
    val colExpr6: ColumnExpression = selectSmt.projections(0).expression.getOrElse(null).asInstanceOf[ColumnExpression]
    colExpr6.columnName._1 should === ("""lineitem""")
    colExpr6.columnName._2 should === ( """orderkey""")

    selectSmt.selection.get.expression shouldBe a [BinaryOperatorExpression]
    val binOperExpr3: BinaryOperatorExpression = selectSmt.selection.get.expression.asInstanceOf[BinaryOperatorExpression]
    binOperExpr3.leftExpression shouldBe a [ColumnExpression]
    val colExpr7: ColumnExpression = binOperExpr3.leftExpression.asInstanceOf[ColumnExpression]
    colExpr7.columnName._1 should === ("""lineitem""")
    colExpr7.columnName._2 should === ( """orderkey""")

    binOperExpr3.binaryOperator shouldBe BinaryOperatorEnum.equalTo
    binOperExpr3.rightExpression shouldBe a [LiteralValueExpression]
    val literValExpr6: LiteralValueExpression = binOperExpr3.rightExpression.asInstanceOf[LiteralValueExpression]
    literValExpr6 shouldBe a [StringLiteral]
    val strLiter2: StringLiteral = literValExpr6.asInstanceOf[StringLiteral]
    strLiter2.value should === ("""key""")

    //AND part/size IN (part/size > 7, part/size < 10)
    binOperExpr2.binaryOperator shouldBe BinaryOperatorEnum.AND
    binOperExpr2.rightExpression shouldBe a [InExpression]
    val inExpr3: InExpression = binOperExpr2.rightExpression.asInstanceOf[InExpression]
    inExpr3.negation shouldBe true
    inExpr3.expression shouldBe a [ColumnExpression]
    val colExpr8: ColumnExpression = inExpr3.expression.asInstanceOf[ColumnExpression]
    colExpr8.columnName._1 should === ("""part""")
    colExpr8.columnName._2 should === ( """size""")

    val exprs2:Array[Expression] = inExpr3.expressions.getOrElse(null)
    exprs2(0) shouldBe a [BinaryOperatorExpression]
    val binOperExpr4: BinaryOperatorExpression = exprs2(0).asInstanceOf[BinaryOperatorExpression]
    binOperExpr4.leftExpression shouldBe a [ColumnExpression]
    val colExpr9: ColumnExpression = binOperExpr4.leftExpression.asInstanceOf[ColumnExpression]
    colExpr9.columnName._1 should === ("""part""")
    colExpr9.columnName._2 should === ( """size""")

    binOperExpr4.binaryOperator shouldBe BinaryOperatorEnum.greaterThan
    binOperExpr4.rightExpression shouldBe a [LiteralValueExpression]
    val literValExpr7: LiteralValueExpression = binOperExpr4.rightExpression.asInstanceOf[LiteralValueExpression]
    literValExpr7 shouldBe a [IntegerLiteral]
    val intLiter4: IntegerLiteral = literValExpr7.asInstanceOf[IntegerLiteral]
    intLiter4.value should === (7)

    exprs2(1) shouldBe a [BinaryOperatorExpression]
    val binOperExpr5: BinaryOperatorExpression = exprs2(1).asInstanceOf[BinaryOperatorExpression]
    binOperExpr5.leftExpression shouldBe a [ColumnExpression]
    val colExpr10: ColumnExpression = binOperExpr5.leftExpression.asInstanceOf[ColumnExpression]
    colExpr10.columnName._1 should === ("""part""")
    colExpr10.columnName._2 should === ( """size""")
    binOperExpr5.binaryOperator shouldBe BinaryOperatorEnum.lessThan
    binOperExpr5.rightExpression shouldBe a [LiteralValueExpression]
    val literValExpr8: LiteralValueExpression = binOperExpr5.rightExpression.asInstanceOf[LiteralValueExpression]
    literValExpr8 shouldBe a [IntegerLiteral]
    val intLiter5: IntegerLiteral = literValExpr8.asInstanceOf[IntegerLiteral]
    intLiter5.value should === (10)

  }

  //SQL 14
  test("sql_14") {
    val result: Either[String, SelectStatement] = Parser.apply(sql_14)
    //print(result.toString())
    result.getOrElse(null) shouldBe a [SelectStatement]
    val smt: SelectStatement = result.getOrElse(null).asInstanceOf[SelectStatement]
    //supplier/name
    smt.projections(0).expression.getOrElse(null) shouldBe a [ColumnExpression]
    val colExpr: ColumnExpression = smt.projections(0).expression.getOrElse(null).asInstanceOf[ColumnExpression]
    colExpr.columnName._1 should === ("""supplier""")
    colExpr.columnName._2 should === ( """name""")

    smt.selection.get.expression shouldBe a [BinaryOperatorExpression]
    val binOperExpr: BinaryOperatorExpression = smt.selection.get.expression.asInstanceOf[BinaryOperatorExpression]
    //EXISTS (
    //    SELECT
    //      *
    //    WHERE
    //      lineitem/orderkey = product/orderkey
    //    )
    binOperExpr.leftExpression shouldBe a [SelectExpression]
    val selectExpr: SelectExpression = binOperExpr.leftExpression.asInstanceOf[SelectExpression]
    selectExpr.selectType.getOrElse(null) shouldBe SelectTypeEnum.exists
    selectExpr.selectStatement.projections(0).asterisk.getOrElse(false) shouldBe true
    selectExpr.selectStatement.selection.get.expression shouldBe a [BinaryOperatorExpression]
    val binOperExpr1: BinaryOperatorExpression = selectExpr.selectStatement.selection.get.expression.asInstanceOf[BinaryOperatorExpression]
    binOperExpr1.leftExpression shouldBe a [ColumnExpression]
    val colExpr1: ColumnExpression = binOperExpr1.leftExpression.asInstanceOf[ColumnExpression]
    colExpr1.columnName._1 should === ("""lineitem""")
    colExpr1.columnName._2 should ===  ("""orderkey""")

    binOperExpr1.binaryOperator shouldBe BinaryOperatorEnum.equalTo
    binOperExpr1.rightExpression shouldBe a [ColumnExpression]
    val colExpr2: ColumnExpression = binOperExpr1.rightExpression.asInstanceOf[ColumnExpression]
    colExpr2.columnName._1 should === ("""product""")
    colExpr2.columnName._2 should === ( """orderkey""")

    //AND NOT EXISTS (
    //    SELECT
    //      *
    //    WHERE
    //      lineitem/receiptdate <> nation/receiptdate
    //  )
    binOperExpr.binaryOperator shouldBe BinaryOperatorEnum.AND
    binOperExpr.rightExpression shouldBe a [SelectExpression]
    val selectExpr1: SelectExpression = binOperExpr.rightExpression.asInstanceOf[SelectExpression]
    selectExpr1.selectType.getOrElse(null) shouldBe SelectTypeEnum.notExists
    selectExpr1.selectStatement.projections(0).asterisk.getOrElse(false) shouldBe true
    selectExpr1.selectStatement.selection.get.expression shouldBe a [BinaryOperatorExpression]
    val binOperExpr2: BinaryOperatorExpression = selectExpr1.selectStatement.selection.get.expression.asInstanceOf[BinaryOperatorExpression]
    binOperExpr2.leftExpression shouldBe a [ColumnExpression]
    val colExpr3: ColumnExpression = binOperExpr2.leftExpression.asInstanceOf[ColumnExpression]
    colExpr3.columnName._1 should === ("""lineitem""")
    colExpr3.columnName._2 should === ( """receiptdate""")

    binOperExpr2.binaryOperator shouldBe BinaryOperatorEnum.notequalTo
    binOperExpr2.rightExpression shouldBe a [ColumnExpression]
    val colExpr4: ColumnExpression = binOperExpr2.rightExpression.asInstanceOf[ColumnExpression]
    colExpr4.columnName._1.equalsIgnoreCase("""nation""")
    colExpr4.columnName._2.equalsIgnoreCase( """receiptdate""")

  }


  //SQL 15
  test("sql_15") {
    val result: Either[String, SelectStatement] = Parser.apply(sql_15)
    //print(result.toString())
    result.getOrElse(null) shouldBe a [SelectStatement]
    val smt: SelectStatement = result.getOrElse(null).asInstanceOf[SelectStatement]
    //lineitem/returnflag
    smt.projections(0).expression.getOrElse(null) shouldBe a [ColumnExpression]
    val colExpr: ColumnExpression = smt.projections(0).expression.getOrElse(null).asInstanceOf[ColumnExpression]
    colExpr.columnName._1 should === ("""lineitem""")
    colExpr.columnName._2 should === ( """returnflag""")

    //lineitem/linestatus
    smt.projections(1).expression.getOrElse(null) shouldBe a [ColumnExpression]
    val colExpr1: ColumnExpression = smt.projections(1).expression.getOrElse(null).asInstanceOf[ColumnExpression]
    colExpr1.columnName._1 should === ("""lineitem""")
    colExpr1.columnName._2 should === ( """linestatus""")

    // product/orderkey in (
    //        SELECT
    //            product/orderkey
    //        GROUP BY
    //           product/orderkey HAVING SUM(lineitem/quantity) > 330
    //        )
    smt.selection.get.expression shouldBe a [InExpression]
    val inExpr: InExpression = smt.selection.get.expression.asInstanceOf[InExpression]
    inExpr.negation shouldBe true
    inExpr.expression shouldBe a [ColumnExpression]
    val colExpr2: ColumnExpression = inExpr.expression.asInstanceOf[ColumnExpression]
    colExpr2.columnName._1 should === ("""product""")
    colExpr2.columnName._2 should === ( """orderkey""")

    val selectSmt: SelectStatement = inExpr.selectStatement.getOrElse(null)

    val groupby: GroupBy = selectSmt.groupBy.getOrElse(null)
    val exprs: Array[Expression] = groupby.groupExpressions
    exprs(0) shouldBe a [ColumnExpression]
    val colExpr3: ColumnExpression = exprs(0).asInstanceOf[ColumnExpression]
    colExpr3.columnName._1 should === ("""product""")
    colExpr3.columnName._2 should === ( """orderkey""")

    groupby.havingExpression.getOrElse(null) shouldBe a [BinaryOperatorExpression]
    val binOperExpr: BinaryOperatorExpression = groupby.havingExpression.getOrElse(null).asInstanceOf[BinaryOperatorExpression]
    binOperExpr.leftExpression shouldBe a [ExpressionFunction]
    val funcExpr: ExpressionFunction = binOperExpr.leftExpression.asInstanceOf[ExpressionFunction]
    funcExpr.functionName should === ("""SUM""")
    val exprs1: Array[Expression] = funcExpr.expressions.getOrElse(null)
    exprs1(0) shouldBe a [ColumnExpression]
    val colExpr4: ColumnExpression = exprs1(0).asInstanceOf[ColumnExpression]
    colExpr4.columnName._1 should === ("""lineitem""")
    colExpr4.columnName._2 should === ( """quantity""")

    binOperExpr.binaryOperator === BinaryOperatorEnum.greaterThan
    binOperExpr.rightExpression shouldBe a [LiteralValueExpression]
    val literValExpr: LiteralValueExpression = binOperExpr.rightExpression.asInstanceOf[LiteralValueExpression]
    literValExpr shouldBe a [IntegerLiteral]
    val intLiter: IntegerLiteral = literValExpr.asInstanceOf[IntegerLiteral]
    intLiter.value should === (330)

    //GROUP BY
    //    l_returnflag,
    //    l_linestatus
    val groupby1: GroupBy = smt.groupBy.getOrElse(null)
    val exprs2: Array[Expression] = groupby1.groupExpressions
    exprs2(0) shouldBe a [ColumnExpression]
    val colExpr5: ColumnExpression = exprs2(0).asInstanceOf[ColumnExpression]
    colExpr5.columnName._1 should === ("""l_returnflag""")

    exprs2(1) shouldBe a [ColumnExpression]
    val colExpr6: ColumnExpression = exprs2(1).asInstanceOf[ColumnExpression]
    colExpr6.columnName._1 should === ("""l_linestatus""")

  }

  //SQL 16
  test("sql_16") {
    val result: Either[String, SelectStatement] = Parser.apply(sql_16)
    //print(result.toString())
    result.getOrElse(null) shouldBe a [SelectStatement]
    val smt: SelectStatement = result.getOrElse(null).asInstanceOf[SelectStatement]
    //supplier/name
    smt.projections(0).expression.getOrElse(null) shouldBe a [ColumnExpression]
    val colExpr: ColumnExpression = smt.projections(0).expression.getOrElse(null).asInstanceOf[ColumnExpression]
    colExpr.columnName._1 should === ("""supplier""")
    colExpr.columnName._2 should === ( """name""")

    //supplier/address
    smt.projections(1).expression.getOrElse(null).isInstanceOf[ColumnExpression]
    val colExpr1: ColumnExpression = smt.projections(1).expression.getOrElse(null).asInstanceOf[ColumnExpression]
    colExpr1.columnName._1 should === ("""supplier""")
    colExpr1.columnName._2 should === ( """address""")

    //product/orderstatus = “F”
    smt.selection.get.expression shouldBe a [BinaryOperatorExpression]
    val binOperExpr: BinaryOperatorExpression = smt.selection.get.expression.asInstanceOf[BinaryOperatorExpression]
    binOperExpr.leftExpression shouldBe a [ColumnExpression]
    val colExpr2: ColumnExpression = binOperExpr.leftExpression.asInstanceOf[ColumnExpression]
    colExpr2.columnName._1 should === ("""product""")
    colExpr2.columnName._2 should === ( """orderstatus""")

    binOperExpr.binaryOperator shouldBe BinaryOperatorEnum.equalTo
    binOperExpr.rightExpression shouldBe a [LiteralValueExpression]
    val literValExpr: LiteralValueExpression = binOperExpr.rightExpression.asInstanceOf[LiteralValueExpression]
    literValExpr shouldBe a [StringLiteral]
    val strLiter: StringLiteral = literValExpr.asInstanceOf[StringLiteral]
    strLiter.value should === ("""F""")
    //supplier/name
    val orderby = smt.orderBy.getOrElse(null)
    val orderTerm:Array[OrderingTerm] = orderby.orderingTerms
    orderTerm(0).expression shouldBe a [ColumnExpression]
    val colExpr3: ColumnExpression = orderTerm(0).expression.asInstanceOf[ColumnExpression]
    colExpr3.columnName._1 should === ("""supplier""")
    colExpr3.columnName._2 should === ( """name""")

  }

  //SQL 17
  test("sql_17") {
    val result: Either[String, SelectStatement] = Parser.apply(sql_17)
    //print(result.toString())
    result.getOrElse(null) shouldBe a [SelectStatement]
    val smt: SelectStatement = result.getOrElse(null).asInstanceOf[SelectStatement]
    //supplier/name
    smt.projections(0).expression.getOrElse(null) shouldBe a [ColumnExpression]
    val colExpr: ColumnExpression = smt.projections(0).expression.getOrElse(null).asInstanceOf[ColumnExpression]
    colExpr.columnName._1 should === ("""supplier""")
    colExpr.columnName._2 should === ( """name""")

    //product/orderstatus = “F”
    smt.selection.get.expression shouldBe a [BinaryOperatorExpression]
    val binOperExpr: BinaryOperatorExpression = smt.selection.get.expression.asInstanceOf[BinaryOperatorExpression]
    binOperExpr.leftExpression shouldBe a [ColumnExpression]
    val colExpr1: ColumnExpression = binOperExpr.leftExpression.asInstanceOf[ColumnExpression]
    colExpr1.columnName._1 should === ("""product""")
    colExpr1.columnName._2 should === ( """orderstatus""")

    binOperExpr.binaryOperator shouldBe BinaryOperatorEnum.equalTo
    binOperExpr.rightExpression shouldBe a [LiteralValueExpression]
    val literValExpr: LiteralValueExpression = binOperExpr.rightExpression.asInstanceOf[LiteralValueExpression]
    literValExpr shouldBe a [StringLiteral]
    val strLiter: StringLiteral = literValExpr.asInstanceOf[StringLiteral]
    strLiter.value should === ("""F""")

    //supplier/name COLLATE Traditional_name ASC NULLS FIRST
    val orderby = smt.orderBy.getOrElse(null)
    val orderTerm:Array[OrderingTerm] = orderby.orderingTerms
    orderTerm(0).expression shouldBe a [CollateExpression]
    val collExpr:CollateExpression = orderTerm(0).expression.asInstanceOf[CollateExpression]
    collExpr.collateExpression shouldBe a [ColumnExpression]
    val colExpr2: ColumnExpression = collExpr.collateExpression.asInstanceOf[ColumnExpression]
    colExpr2.columnName._1 should === ("""supplier""")
    colExpr2.columnName._2 should === ( """name""")
    collExpr.collationName should === ("""Traditional_name""")

    orderTerm(0).order.getOrElse(null) shouldBe OrderEnum.asc
    orderTerm(0).nullOrder.getOrElse(null) shouldBe NullOrderEnum.nullsFirst
  }

  //SQL 18
  test("sql_18") {
    val result: Either[String, SelectStatement] = Parser.apply(sql_18)
    //print(result.toString())
    result.getOrElse(null) shouldBe a [SelectStatement]
    val smt: SelectStatement = result.getOrElse(null).asInstanceOf[SelectStatement]
    //sum(customer/acctbal) as total_acctbal
    smt.projections(0).expression.getOrElse(null).isInstanceOf[ExpressionFunction]
    val funcExpr: ExpressionFunction = smt.projections(0).expression.getOrElse(null).asInstanceOf[ExpressionFunction]
    funcExpr.functionName should === ("""SUM""")
    val exprs: Array[Expression] = funcExpr.expressions.getOrElse(null)
    exprs(0) shouldBe a [ColumnExpression]
    val colExpr: ColumnExpression = exprs(0).asInstanceOf[ColumnExpression]
    colExpr.columnName._1 should === ("""customer""")
    colExpr.columnName._2 should === ( """acctbal""")

    smt.projections(0).alias.getOrElse(null) should === ("""total_acctbal""")

    //customer/phone in (55, 66, 77)
    smt.selection.get.expression shouldBe a [InExpression]
    val inExpr:InExpression = smt.selection.get.expression.asInstanceOf[InExpression]
    inExpr.negation shouldBe true
    inExpr.expression shouldBe a [ColumnExpression]
    val colExpr1: ColumnExpression = inExpr.expression.asInstanceOf[ColumnExpression]
    colExpr1.columnName._1 should === ("""customer""")
    colExpr1.columnName._2 should === ( """phone""")

    val exprs2:Array[Expression] = inExpr.expressions.getOrElse(null)
    exprs2(0) shouldBe a [LiteralValueExpression]
    val literValExpr: LiteralValueExpression = exprs2(0).asInstanceOf[LiteralValueExpression]
    literValExpr shouldBe a [IntegerLiteral]
    val intLiter: IntegerLiteral = literValExpr.asInstanceOf[IntegerLiteral]
    intLiter.value should === (55)

    exprs2(1) shouldBe a [LiteralValueExpression]
    val literValExpr1: LiteralValueExpression = exprs2(1).asInstanceOf[LiteralValueExpression]
    literValExpr1 shouldBe a [IntegerLiteral]
    val intLiter1: IntegerLiteral = literValExpr1.asInstanceOf[IntegerLiteral]
    intLiter1.value should === (66)

    exprs2(2) shouldBe a [LiteralValueExpression]
    val literValExpr2: LiteralValueExpression = exprs2(2).asInstanceOf[LiteralValueExpression]
    literValExpr2 shouldBe a [IntegerLiteral]
    val intLiter2: IntegerLiteral = literValExpr2.asInstanceOf[IntegerLiteral]
    intLiter2.value should === (77)

    //customer/name COLLATE Traditional_name DESC NULLS LAST
    val orderby = smt.orderBy.getOrElse(null)
    val orderTerm:Array[OrderingTerm] = orderby.orderingTerms
    orderTerm(0).expression shouldBe a [CollateExpression]
    val collExpr:CollateExpression = orderTerm(0).expression.asInstanceOf[CollateExpression]
    collExpr.collateExpression shouldBe a [ColumnExpression]
    val colExpr2: ColumnExpression = collExpr.collateExpression.asInstanceOf[ColumnExpression]
    colExpr2.columnName._1 should === ("""customer""")
    colExpr2.columnName._2 should === ( """name""")

    collExpr.collationName should === ("""Traditional_name""")
    orderTerm(0).order.getOrElse(null) shouldBe OrderEnum.desc
    orderTerm(0).nullOrder.getOrElse(null) shouldBe NullOrderEnum.nullsLast

  }

  //SQL 19
  test("sql_19") {
    val result: Either[String, SelectStatement] = Parser.apply(sql_19)
    //print(result.toString())
    result.getOrElse(null) shouldBe a [SelectStatement]

    val smt: SelectStatement = result.getOrElse(null).asInstanceOf[SelectStatement]
    //SUM(lineitem/extendedpricen * (1 - lineitem/discount)) as revenue
    smt.projections(0).expression.getOrElse(null) shouldBe a [ExpressionFunction]
    val funcExpr: ExpressionFunction = smt.projections(0).expression.getOrElse(null).asInstanceOf[ExpressionFunction]
    funcExpr.functionName should === ("""SUM""")
    val exprs: Array[Expression] = funcExpr.expressions.getOrElse(null)
    exprs(0) shouldBe a [BinaryOperatorExpression]
    val binOperExpr = exprs(0).asInstanceOf[BinaryOperatorExpression]
    binOperExpr.leftExpression shouldBe a [ColumnExpression]
    val colExpr: ColumnExpression = binOperExpr.leftExpression.asInstanceOf[ColumnExpression]
    colExpr.columnName._1 should === ("""lineitem""")
    colExpr.columnName._2 should === ( """extendedpricen""")

    binOperExpr.binaryOperator shouldBe BinaryOperatorEnum.multiply
    binOperExpr.rightExpression shouldBe a [BinaryOperatorExpression]
    val binOperExpr1 = binOperExpr.rightExpression.asInstanceOf[BinaryOperatorExpression]
    binOperExpr1.leftExpression shouldBe a [LiteralValueExpression]
    val literValExpr: LiteralValueExpression = binOperExpr1.leftExpression.asInstanceOf[LiteralValueExpression]
    literValExpr shouldBe a [IntegerLiteral]
    val intLiter: IntegerLiteral = literValExpr.asInstanceOf[IntegerLiteral]
    intLiter.value should === (1)

    binOperExpr1.binaryOperator shouldBe BinaryOperatorEnum.subtract
    binOperExpr1.rightExpression.isInstanceOf[ColumnExpression]
    val colExpr1: ColumnExpression = binOperExpr1.rightExpression.asInstanceOf[ColumnExpression]
    colExpr1.columnName._1 should === ("""lineitem""")
    colExpr1.columnName._2 should === ( """discount""")

    smt.projections(0).alias.getOrElse(null) should === ("""revenue""")
    //customer/name
    smt.projections(1).expression.getOrElse(null).isInstanceOf[ColumnExpression]
    val colExpr2: ColumnExpression = smt.projections(1).expression.getOrElse(null).asInstanceOf[ColumnExpression]
    colExpr2.columnName._1 should === ("""customer""")
    colExpr2.columnName._2 should === ( """name""")

    //part/container in ('MED BAG', 'MED BOX')
    smt.selection.get.expression shouldBe a [InExpression]
    val inExpr:InExpression = smt.selection.get.expression.asInstanceOf[InExpression]
    inExpr.negation shouldBe true
    inExpr.expression shouldBe a [ColumnExpression]
    val colExpr3: ColumnExpression = inExpr.expression.asInstanceOf[ColumnExpression]
    colExpr3.columnName._1 should === ("""part""")
    colExpr3.columnName._2 should === ( """container""")

    val exprs1:Array[Expression] = inExpr.expressions.getOrElse(null)
    exprs1(0) shouldBe a [LiteralValueExpression]
    val literValExpr1: LiteralValueExpression = exprs1(0).asInstanceOf[LiteralValueExpression]
    literValExpr1 shouldBe a [StringLiteral]
    val strLiter: StringLiteral = literValExpr1.asInstanceOf[StringLiteral]
    strLiter.value should === ("""MED BAG""")

    exprs1(1) shouldBe a [LiteralValueExpression]
    val literValExpr2: LiteralValueExpression = exprs1(1).asInstanceOf[LiteralValueExpression]
    literValExpr2 shouldBe a [StringLiteral]
    val strLiter1: StringLiteral = literValExpr2.asInstanceOf[StringLiteral]
    strLiter1.value should === ("""MED BOX""")

    //customer/name
    val groupby: GroupBy = smt.groupBy.getOrElse(null)
    val exprs2: Array[Expression] = groupby.groupExpressions
    exprs2(0) shouldBe a [ColumnExpression]
    val colExpr4: ColumnExpression = exprs2(0).asInstanceOf[ColumnExpression]
    colExpr4.columnName._1 should === ("""customer""")
    colExpr4.columnName._2 should === ( """name""")

    //customer/orderdate ASC
    val orderby = smt.orderBy.getOrElse(null)
    val orderTerm:Array[OrderingTerm] = orderby.orderingTerms
    orderTerm(0).expression shouldBe a [ColumnExpression]
    val colExpr5: ColumnExpression = orderTerm(0).expression.asInstanceOf[ColumnExpression]
    colExpr5.columnName._1 should === ("""customer""")
    colExpr5.columnName._2 should === ( """orderdate""")

    orderTerm(0).order.getOrElse(null) shouldBe OrderEnum.asc
    //LIMIT 10
    val limit: Limit = smt.limit.getOrElse(null)
    limit.limitExpression shouldBe a [LiteralValueExpression]
    val literValExpr3: LiteralValueExpression = limit.limitExpression.asInstanceOf[LiteralValueExpression]
    literValExpr3 shouldBe a [IntegerLiteral]
    val intLiter2: IntegerLiteral = literValExpr3.asInstanceOf[IntegerLiteral]
    intLiter2.value should === (10)

  }

  //SQL 20
  test("sql_20") {
    val result: Either[String, SelectStatement] = Parser.apply(sql_20)
    //print(result.toString())
    result.getOrElse(null) shouldBe a [SelectStatement]
    val smt: SelectStatement = result.getOrElse(null).asInstanceOf[SelectStatement]
    //supplier/address
    smt.projections(0).expression.getOrElse(null).isInstanceOf[ColumnExpression]
    val colExpr6: ColumnExpression = smt.projections(0).expression.getOrElse(null).asInstanceOf[ColumnExpression]
    colExpr6.columnName._1 should === ("""supplier""")
    colExpr6.columnName._2 should === ( """address""")

    //COUNT(*) as numwait
    smt.projections(1).expression.getOrElse(null) shouldBe a [AsteriskFunction]
    val astfuncExpr: AsteriskFunction = smt.projections(1).expression.getOrElse(null).asInstanceOf[AsteriskFunction]
    astfuncExpr.functionName should === ("""COUNT""")

    smt.projections(1).alias.getOrElse(null) should === ("""numwait""")
    //lineitem/quantity >= 5 AND lineitem/quantity <= 5 + 10
    smt.selection.get.expression shouldBe a [BinaryOperatorExpression]
    val binOperExpr: BinaryOperatorExpression = smt.selection.get.expression.asInstanceOf[BinaryOperatorExpression]
    binOperExpr.leftExpression shouldBe a [BinaryOperatorExpression]
    val binOperExpr1:BinaryOperatorExpression = binOperExpr.leftExpression.asInstanceOf[BinaryOperatorExpression]
    binOperExpr1.leftExpression shouldBe a [ColumnExpression]
    val colExpr7: ColumnExpression = binOperExpr1.leftExpression.asInstanceOf[ColumnExpression]
    colExpr7.columnName._1 should === ("""lineitem""")
    colExpr7.columnName._2 should === ( """quantity""")
    binOperExpr1.binaryOperator shouldBe BinaryOperatorEnum.greaterThanOrEqualTo
    binOperExpr1.rightExpression shouldBe a [LiteralValueExpression]
    val literValExpr: LiteralValueExpression = binOperExpr1.rightExpression.asInstanceOf[LiteralValueExpression]
    literValExpr shouldBe a [IntegerLiteral]
    val intLiter: IntegerLiteral = literValExpr.asInstanceOf[IntegerLiteral]
    intLiter.value should === (5)

    binOperExpr.binaryOperator shouldBe BinaryOperatorEnum.AND

    binOperExpr.rightExpression shouldBe a [BinaryOperatorExpression]
    val binOperExpr2:BinaryOperatorExpression = binOperExpr.rightExpression.asInstanceOf[BinaryOperatorExpression]
    binOperExpr2.leftExpression shouldBe a [ColumnExpression]
    val colExpr8: ColumnExpression = binOperExpr1.leftExpression.asInstanceOf[ColumnExpression]
    colExpr8.columnName._1 should === ("""lineitem""")
    colExpr8.columnName._2 should === ( """quantity""")

    binOperExpr2.binaryOperator shouldBe BinaryOperatorEnum.lessThanOrEqualTo
    binOperExpr2.rightExpression shouldBe a [BinaryOperatorExpression]
    val binOperExpr3:BinaryOperatorExpression = binOperExpr2.rightExpression.asInstanceOf[BinaryOperatorExpression]
    binOperExpr3.leftExpression shouldBe a [LiteralValueExpression]
    val literValExpr1: LiteralValueExpression = binOperExpr3.leftExpression.asInstanceOf[LiteralValueExpression]
    literValExpr1 shouldBe a [IntegerLiteral]
    val intLiter1: IntegerLiteral = literValExpr1.asInstanceOf[IntegerLiteral]
    intLiter1.value should === (5)

    binOperExpr3.binaryOperator shouldBe BinaryOperatorEnum.add
    binOperExpr3.rightExpression shouldBe a [LiteralValueExpression]
    val literValExpr2: LiteralValueExpression = binOperExpr3.rightExpression.asInstanceOf[LiteralValueExpression]
    literValExpr2 shouldBe a [IntegerLiteral]
    val intLiter3: IntegerLiteral = literValExpr2.asInstanceOf[IntegerLiteral]
    intLiter3.value should === (10)

    //supplier/address
    val groupby: GroupBy = smt.groupBy.getOrElse(null)
    val exprs: Array[Expression] = groupby.groupExpressions
    exprs(0) shouldBe a [ColumnExpression]
    val colExpr: ColumnExpression = exprs(0).asInstanceOf[ColumnExpression]
    colExpr.columnName._1 should === ("""supplier""")
    colExpr.columnName._2 should === ( """address""")

    //numwait DESC
    val orderby = smt.orderBy.getOrElse(null)
    val orderTerm:Array[OrderingTerm] = orderby.orderingTerms
    orderTerm(0).expression shouldBe a [ColumnExpression]
    val colExpr1: ColumnExpression = orderTerm(0).expression.asInstanceOf[ColumnExpression]
    colExpr1.columnName._1 should === ( """numwait""")

    orderTerm(0).order.getOrElse(null) shouldBe OrderEnum.desc
    //LIMIT 5 OFFSET 3
    val limit: Limit = smt.limit.getOrElse(null)
    limit.limitExpression shouldBe a [LiteralValueExpression]
    val literValExpr3: LiteralValueExpression = limit.limitExpression.asInstanceOf[LiteralValueExpression]
    literValExpr3 shouldBe a [IntegerLiteral]
    val intLiter2: IntegerLiteral = literValExpr.asInstanceOf[IntegerLiteral]
    intLiter2.value should === (5)

    limit.offsetExpression.getOrElse(null) shouldBe a [LiteralValueExpression]
    val literValExpr4: LiteralValueExpression = limit.offsetExpression.getOrElse(null).asInstanceOf[LiteralValueExpression]
    literValExpr4 shouldBe a [IntegerLiteral]
    val intLiter4: IntegerLiteral = literValExpr4.asInstanceOf[IntegerLiteral]
    intLiter4.value should === (3)

  }
/*
//window part, not optimal now (to do)
  //SQL 23
  test("sql_23") {
    val result: Either[String, SelectStatement] = Parser.apply(sql_23)

    if (result.right.isInstanceOf[SelectStatement]) {
      val smt: SelectStatement = result.right.asInstanceOf[SelectStatement]
      //AVG(part/size) OVER (
      //                          PARTITION BY part/type
      //                          ORDER BY part/name
      //                         ) AS part_avg
      if (smt.projections(0).expression.isInstanceOf[ExpressionFunction]) {
        val funcExpr: ExpressionFunction = smt.projections(0).expression.asInstanceOf[ExpressionFunction]
        assert(funcExpr.functionName === """AVG""")
        val exprs: Array[Expression] = funcExpr.expressions.getOrElse(null)
        if (exprs(0).isInstanceOf[ColumnExpression]) {
          val colExpr: ColumnExpression = exprs(0).asInstanceOf[ColumnExpression]
          assert(colExpr.columnName === """size""")
        }
        val overClause = funcExpr.overClause.getOrElse(null)
        val winDef = overClause.windowDefinition.getOrElse(null)
        val partitionExprs = winDef.partitionExpressions.getOrElse(null)
        if(partitionExprs(0).isInstanceOf[ColumnExpression]){
          val colExpr: ColumnExpression = partitionExprs(0).asInstanceOf[ColumnExpression]
          assert(colExpr.columnName === """type""")
        }
        val orderingTerms = winDef.orderingTerms.getOrElse(null)
        if(orderingTerms(0).isInstanceOf[ColumnExpression]){
          val colExpr: ColumnExpression = orderingTerms(0).asInstanceOf[ColumnExpression]
          assert(colExpr.columnName === """name""")
        }
      }
      assert(smt.projections(0).alias === """part_avg""")

      //region/name = 'somename'
      if (smt.selection.get.isInstanceOf[BinaryOperatorExpression]) {
        val binOperExpr: BinaryOperatorExpression = smt.selection.get.asInstanceOf[BinaryOperatorExpression]
        if (binOperExpr.leftExpression.isInstanceOf[ColumnExpression]) {
          val colExpr: ColumnExpression = binOperExpr.leftExpression.asInstanceOf[ColumnExpression]
          assert(colExpr.columnName === """name""")
        }
        assert(binOperExpr.binaryOperator === BinaryOperatorEnum.equalTo)
        if (binOperExpr.rightExpression.isInstanceOf[LiteralValueExpression]) {
          val literValExpr: LiteralValueExpression = binOperExpr.rightExpression.asInstanceOf[LiteralValueExpression]
          if (literValExpr.isInstanceOf[StringLiteral]) {
            val strLiter: StringLiteral = literValExpr.asInstanceOf[StringLiteral]
            assert(strLiter.value === """somename""")
          }
        }
      }
    }
  }

  //SQL 24
  test("sql_24") {
    val result: Either[String, SelectStatement] = Parser.apply(sql_24)

    if (result.right.isInstanceOf[SelectStatement]) {
      val smt: SelectStatement = result.right.asInstanceOf[SelectStatement]
      //order/date
      if (smt.projections(0).expression.isInstanceOf[ColumnExpression]) {
        val colExpr: ColumnExpression = smt.projections(0).expression.asInstanceOf[ColumnExpression]
        assert(colExpr.columnName === """date""")
      }
      //AVG(order/amount) OVER (
      //                             PARTITION BY order/type
      //                             ORDER BY order/date ASC
      //                             RANGE BETWEEN INTERVAL '1' DAY PRECEDING AND CURRENT ROW
      //                            ) AS amount_avg,
      if (smt.projections(1).expression.isInstanceOf[ExpressionFunction]) {
        val funcExpr: ExpressionFunction = smt.projections(1).expression.asInstanceOf[ExpressionFunction]
        assert(funcExpr.functionName === """AVG""")
        val exprs: Array[Expression] = funcExpr.expressions.getOrElse(null)
        if(exprs(0).isInstanceOf[ColumnExpression]) {
          val colExpr: ColumnExpression = exprs(0).asInstanceOf[ColumnExpression]
          assert(colExpr.columnName === """amount""")
        }
        val overClause = funcExpr.overClause.getOrElse(null)
        val winDef = overClause.windowDefinition.getOrElse(null)
        val partitionExprs = winDef.partitionExpressions.getOrElse(null)
        if(partitionExprs(0).isInstanceOf[ColumnExpression]){
          val colExpr: ColumnExpression = partitionExprs(0).asInstanceOf[ColumnExpression]
          assert(colExpr.columnName === """type""")
        }
        val orderingTerms = winDef.orderingTerms.getOrElse(null)
        if(orderingTerms(0).isInstanceOf[ColumnExpression]){
          val colExpr: ColumnExpression = orderingTerms(0).asInstanceOf[ColumnExpression]
          assert(colExpr.columnName === """date""")
        }
        assert(orderingTerms(0).order === OrderEnum.asc)
        //to do FrameBetween
        //to do frame.precedingExpression INTERVAL '1' DAY ?
      }
      assert(smt.projections(0).alias === """amount_avg""")
      //COUNT(*) OVER (
      //                   ORDER BY order/id
      //                   GROUPS BETWEEN 2 PRECEDING AND 2 FOLLOWING
      //                  )AS order_number,
      if (smt.projections(1).expression.isInstanceOf[AsteriskFunction]) {
        val astfuncExpr: AsteriskFunction = smt.projections(1).expression.asInstanceOf[AsteriskFunction]
        assert(astfuncExpr.functionName === """COUNT""")
        val overClause = astfuncExpr.overClause.getOrElse(null)
        val winDef = overClause.windowDefinition.getOrElse(null)
        val orderingTerms = winDef.orderingTerms.getOrElse(null)
        if(orderingTerms(0).isInstanceOf[ColumnExpression]){
          val colExpr: ColumnExpression = orderingTerms(0).asInstanceOf[ColumnExpression]
          assert(colExpr.columnName === """id""")
        }
        //to do FrameBetween
      }
      assert(smt.projections(0).alias === """order_number""")
      //    COUNT(*) OVER (
      //                   order by supplier/id
      //                   ROWS BETWEEN CURRENT ROW AND UNBOUNDED FOLLOWING
      //                   )AS supplier_number
      if (smt.projections(1).expression.isInstanceOf[AsteriskFunction]) {
        val astfuncExpr: AsteriskFunction = smt.projections(1).expression.asInstanceOf[AsteriskFunction]
        assert(astfuncExpr.functionName === """COUNT""")
        val overClause = astfuncExpr.overClause.getOrElse(null)
        val winDef = overClause.windowDefinition.getOrElse(null)
        val orderingTerms = winDef.orderingTerms.getOrElse(null)
        if(orderingTerms(0).isInstanceOf[ColumnExpression]){
          val colExpr: ColumnExpression = orderingTerms(0).asInstanceOf[ColumnExpression]
          assert(colExpr.columnName === """id""")
        }
        //to do FrameBetween
      }
      assert(smt.projections(0).alias === """supplier_number""")
    }
  }
*/
}



//***************************************************************************************
// End of File
//***************************************************************************************
