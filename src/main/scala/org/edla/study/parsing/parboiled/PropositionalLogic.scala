package org.edla.study.parsing.parboiled

import scala.language.implicitConversions

import org.edla.port.atp.Formulas._
import org.parboiled2._

class PropositionalLogic(val input: ParserInput) extends Parser {

  implicit def wspStr(s: String): Rule0 = rule {
    str(s) ~ zeroOrMore(WhiteSpaceChar)
  }

  def expr = equiv

  def equiv = rule {
    oneOrMore(impl).separatedBy("<=>") ~> (_.reduceLeft(Iff))
  }

  def impl = rule {
    oneOrMore(or).separatedBy("==>") ~> (_.reduceRight(Imp))
  }

  def or = rule {
    oneOrMore(and).separatedBy("""\/""") ~> (_.reduceLeft(Or))
  }

  def and = rule {
    oneOrMore(not).separatedBy("""/\""") ~> (_.reduceLeft(And))
  }

  def not: Rule1[Formula] = rule {
    optional(neg) ~ atom ~> (((a: Option[String], b: Formula) ⇒ if (a.isDefined) Not(b) else b))
  }

  def atom: Rule1[Formula] = rule {
    PTrue | PFalse | ((optional(neg) ~ id ~>
      (((a: Option[String], b: String) ⇒ if (a.isDefined) Not(Atom(b)) else Atom(b)))) | "(" ~ equiv ~ (")" | EOI))
  }

  def neg = rule { capture(("~")) ~> (_.toString) }

  def PTrue: Rule1[Formula] = rule { "true" ~ push(True) }

  def PFalse: Rule1[Formula] = rule { "false" ~ push(False) }

  def id = rule { capture(oneOrMore("a" - "z" | "A" - "Z" | "'")) ~ optional(WhiteSpace) ~> (_.toString.replaceAll("""(?m)\s+$""", "")) }

  def numericLit = rule { capture(oneOrMore("0" - "9")) ~ optional(WhiteSpace) ~> (_.toInt) }

  def WhiteSpace = rule { zeroOrMore(WhiteSpaceChar) }

  val WhiteSpaceChar = CharPredicate(" \n\r\t\f")

}