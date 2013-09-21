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
import org.parboiled.scala.ANY
import org.parboiled.scala.Parser
import org.parboiled.scala.Rule1
import org.parboiled.scala.creator4Rule0
import org.parboiled.scala.creator4Rule1

class PropositionalLogic extends Parser {

  def expr = xor

  def xor = rule {
    oneOrMore(equiv, separator = "^ ") ~~> (_.reduceLeft(Xor))
  }

  def equiv = rule {
    oneOrMore(impl, separator = "<=> ") ~~> (_.reduceLeft(Equiv))
  }

  def impl = rule {
    oneOrMore(or, separator = "==> ") ~~> (_.reduceRight(Impl))
  }

  def or = rule {
    oneOrMore(and, separator = """\/ """) ~~> (_.reduceLeft(Or))
  }

  def and = rule {
    oneOrMore(not, separator = """/\ """) ~~> (_.reduceLeft(And))
  }

  def not: Rule1[Expr] = {
    optional(neg) ~ atom ~ WhiteSpace ~~> { case (Some(n), m) ⇒ Not(m) case (None, m) ⇒ m }
  }

  def atom: Rule1[Expr] = rule {
    (const ~~> Const | (id ~~> Id) | "( " ~ expr ~ ") ")
  }

  def neg = rule { ("~") ~> (_.toString) }

  def const = rule { ("true " | "false ") ~> (_.toString) }

  def id = rule { oneOrMore("a" - "z") ~> (_.toString) }

  def ident = rule { oneOrMore("a" - "z" | "A" - "Z") ~> (_.toString) }

  def numericLit = rule { oneOrMore("0" - "9") ~> (_.toInt) }

  //def NormalChar = rule { !anyOf("\"\\") ~ ANY }

  def WhiteSpace = rule { zeroOrMore(anyOf(" \n\r\t\f")) }

  /**
   * We redefine the default string-to-rule conversion to also match trailing whitespace if the string ends with
   * a blank, this keeps the rules free from most whitespace matching clutter
   */
  override implicit def toRule(string: String) =
    if (string.endsWith(" "))
      str(string.trim) ~ WhiteSpace
    else
      str(string)

}