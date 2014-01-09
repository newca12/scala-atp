package org.edla.study.parsing.parboiled

import scala.language.implicitConversions

import org.edla.study.parsing.parboiled.AST.And
import org.edla.study.parsing.parboiled.AST.Const
import org.edla.study.parsing.parboiled.AST.Equiv
import org.edla.study.parsing.parboiled.AST.Expr
import org.edla.study.parsing.parboiled.AST.Id
import org.edla.study.parsing.parboiled.AST.Impl
import org.edla.study.parsing.parboiled.AST.Not
import org.edla.study.parsing.parboiled.AST.Or
import org.edla.study.parsing.parboiled.AST.Xor
import org.parboiled2._

class PropositionalLogic(val input: ParserInput) extends Parser {

  def expr = xor

  def xor = rule {
    oneOrMore(equiv).separatedBy("^ ") ~> (_.reduceLeft(Xor))
  }

  def equiv = rule {
    oneOrMore(impl).separatedBy("<=> ") ~> (_.reduceLeft(Equiv))
  }

  def impl = rule {
    oneOrMore(or).separatedBy("==> ") ~> (_.reduceRight(Impl))
  }

  def or = rule {
    oneOrMore(and).separatedBy("""\/ """) ~> (_.reduceLeft(Or))
  }

  def and = rule {
    oneOrMore(not).separatedBy("""/\ """) ~> (_.reduceLeft(And))
  }

  def not: Rule1[Expr] = rule {
    optional(neg) ~ atom ~ WhiteSpace ~> (((a: Option[String], b: Expr) ⇒ if (a.isDefined) Not(b) else b))
  }

  def atom: Rule1[Expr] = rule {
    (const ~> Const | (id ~> Id) | ws('(') ~ expr ~ ws(')'))
  }

  def neg = rule { capture(("~")) ~> (_.toString) }

  def const = rule { capture(("true " | "false ")) ~> (_.toString) }

  def id = rule { capture(oneOrMore("a" - "z")) ~> (_.toString) }

  def ident = rule { capture(oneOrMore("a" - "z" | "A" - "Z")) ~> (_.toString) }

  def numericLit = rule { capture(oneOrMore("0" - "9")) ~> (_.toInt) }

  def WhiteSpace = rule { zeroOrMore(WhiteSpaceChar) }

  def ws(c: Char) = rule { c ~ WhiteSpace }

  val WhiteSpaceChar = CharPredicate(" \n\r\t\f")

}