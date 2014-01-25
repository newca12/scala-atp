package org.edla.study.parsing.parboiled

import scala.language.implicitConversions

import org.edla.port.atp.Formulas._
import org.parboiled2.CharPredicate
import org.parboiled2.Parser
import org.parboiled2.ParserInput
import org.parboiled2.Rule1

class PropositionalLogic(val input: ParserInput) extends Parser {

  def expr = equiv

  def equiv = rule {
    oneOrMore(impl).separatedBy("<=> ") ~> (_.reduceLeft(Iff))
  }

  def impl = rule {
    oneOrMore(or).separatedBy("==> ") ~> (_.reduceRight(Imp))
  }

  def or = rule {
    oneOrMore(and).separatedBy("""\/ """) ~> (_.reduceLeft(Or))
  }

  def and = rule {
    oneOrMore(not).separatedBy("""/\ """) ~> (_.reduceLeft(And))
  }

  def not: Rule1[Formula] = rule {
    optional(neg) ~ atom ~ WhiteSpace ~> (((a: Option[String], b: Formula) ⇒ if (a.isDefined) Not(b) else b))
  }

  def atom: Rule1[Formula] = rule {
    PTrue | PFalse | ((id ~> Atom) | ws('(') ~ expr ~ ws(')'))
  }

  def neg = rule { capture(("~")) ~> (_.toString) }

  def PTrue: Rule1[Formula] = rule { "true" ~ WhiteSpace ~> True }

  def PFalse: Rule1[Formula] = rule { "false" ~ WhiteSpace ~> False }

  def id = rule { capture(oneOrMore("a" - "z" | "A" - "Z" | "'")) ~> (_.toString) }

  def numericLit = rule { capture(oneOrMore("0" - "9")) ~> (_.toInt) }

  def WhiteSpace = rule { zeroOrMore(WhiteSpaceChar) }

  def ws(c: Char) = rule { c ~ WhiteSpace }

  val WhiteSpaceChar = CharPredicate(" \n\r\t\f")

}