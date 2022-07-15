//import scala.util.matching.Regex
//import scala.util.parsing.combinator.syntactical.*
//import scala.util.parsing.combinator.token.*
//import scala.util.parsing.input.CharArrayReader.EofCh
//
//final private class Parser2 extends StandardTokenParsers {
//  def regex(r: Regex): Parser[String] = acceptMatch(
//    s"matching regex $r",
//    { case lexical.Identifier(s) if r.unapplySeq(s).isDefined => s }
//  )
//
//  lexical.delimiters ++= Seq(
//    "*", ";", "=", "/"
//  )
//
//  lexical.reserved ++= Seq(
//    "select", "where"
//  )
//
//  // identifier: name assigned to programming constructs such as variables
//  lazy val identifier: Parser[Identifier] = ident ^^ {case ident => new Identifier(ident)}
//
//  // literal: values assigned to identifiers
//  lazy val literal: Parser[Literal] = intLiteral | stringLiteral
//  lazy val intLiteral: Parser[IntLiteral] = numericLit ^^ {case i => new IntLiteral(i.toInt)}
//  lazy val stringLiteral: Parser[StringLiteral] = stringLit ^^ {case s => new StringLiteral(s)}
//
//  // attribute: identifiers separated by slahses
//  lazy val attribute: Parser[Attribute] = rep1sep(regex("""[A-Z]+""".r), "/") ^^ {case attr => Attribute(attr)}
//
//  lazy val start = query <~ EOI ^^ {case s => s}
//
//  lazy val query: Parser[Statement] = ("select" ~> projections) ~ opt("where" ~> filters) <~ opt(";") ^^ {
//    case p ~ b ~ f => new Statement(p, b, f)
//  }
//
//  lazy val projections: Parser[Seq[Projection]] = repsep(projection, ",")
//
//  lazy val projection: Parser[Projection] = attribute ^^ {case attr => new NameProjection(attr)}
//
//  lazy val filters: Parser[Seq[Filter]] = rep1(comparison)
//
//  lazy val comparison: Parser[Comparison] = attribute ~ "=" ~ literal ^^ {case left ~ op ~ right => new Comparison(left, op, right)}
//
//  lazy val EOI: Parser[Any] = new Parser[Any] {
//    def apply(in: Input) = {
//      if (in.atEnd) new Success("EOI", in)
//      else Failure("end of input expected", in)
//    }
//  }
//
//  def parse(s: String): Either[String, Statement] = phrase(start)(new lexical.Scanner(s)) match {
//    case Success(matched, _) => Right(matched)
//    case Failure(s, _)       => Left(s)
//    case Error(s, _)         => throw new Exception(s"Parser error: ${s}")
//  }
//}
