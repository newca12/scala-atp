package org.edla.study.parsing.parboiled

import scala.language.implicitConversions

import org.edla.study.parsing.parboiled.AST.And
import org.edla.study.parsing.parboiled.AST.False
import org.edla.study.parsing.parboiled.AST.True
import org.edla.study.parsing.parboiled.AST.Iff
import org.edla.study.parsing.parboiled.AST.Formula
import org.edla.study.parsing.parboiled.AST.Atom
import org.edla.study.parsing.parboiled.AST.Imp
import org.edla.study.parsing.parboiled.AST.Not
import org.edla.study.parsing.parboiled.AST.Or
import org.parboiled2._

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
    ((id ~> Atom) | ws('(') ~ expr ~ ws(')'))
  }

  def neg = rule { capture(("~")) ~> (_.toString) }

  def const = rule { capture(("true " | "false ")) ~> (_.toString) }

  def id = rule { capture(oneOrMore("a" - "z" | "A" - "Z")) ~> (_.toString) }

  def numericLit = rule { capture(oneOrMore("0" - "9")) ~> (_.toInt) }

  def WhiteSpace = rule { zeroOrMore(WhiteSpaceChar) }

  def ws(c: Char) = rule { c ~ WhiteSpace }

  val WhiteSpaceChar = CharPredicate(" \n\r\t\f")

}