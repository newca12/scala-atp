package org.edla.study.parsing.parboiled.semantic

import scala.language.implicitConversions

import org.edla.study.parsing.common.AST.AccountSpec
import org.edla.study.parsing.common.AST.BUY
import org.edla.study.parsing.common.AST.BuySell
import org.edla.study.parsing.common.AST.Items
import org.edla.study.parsing.common.AST.LineItem
import org.edla.study.parsing.common.AST.MAX
import org.edla.study.parsing.common.AST.MIN
import org.edla.study.parsing.common.AST.Order
import org.edla.study.parsing.common.AST.PriceSpec
import org.edla.study.parsing.common.AST.PriceType
import org.edla.study.parsing.common.AST.SELL
import org.edla.study.parsing.common.AST.SecuritySpec
import org.parboiled2._

object OrderDsl {
  def apply(input: String) = new OrderDsl(input)
}

class OrderDsl(val input: ParserInput) extends Parser {

  implicit def wspStr(s: String): Rule0 = rule {
    str(s) ~ WhiteSpace
  }

  def order = rule { items ~ account_spec ~> Order }

  def items = rule { ("(" ~ oneOrMore(line_item).separatedBy(",") ~ ")") ~> Items }

  def line_item: Rule1[LineItem] = rule {
    security_spec ~ buy_sell ~ price_spec ~> (LineItem(_, _, _))
  }

  def buy_sell = rule { "to" ~ (("buy" ~ push(BUY)) | ("sell" ~ push(SELL))) }

  def security_spec = rule {
    numericLit ~ ident ~> ((n: Int, s) ⇒ SecuritySpec(n.toInt, s.toString)) ~ "shares"
  }

  def price_spec: Rule1[PriceSpec] = rule {
    "at" ~ optional(min_max) ~> ((m: Option[PriceType]) ⇒ m) ~ numericLit ~> (PriceSpec(_: Option[PriceType], _: Int))
  }

  def min_max: Rule1[PriceType] = rule { "min" ~ push(MIN) | "max" ~ push(MAX) }

  def account_spec = rule { "for" ~ "account" ~ stringLit ~> AccountSpec }

  def stringLit = rule { "\"" ~ capture(zeroOrMore(NormalChar)) ~> (_.toString) ~ "\"" }

  def ident = rule { capture(oneOrMore("a" - "z" | "A" - "Z")) ~ optional(WhiteSpace) ~> (_.toString) }

  def numericLit = rule { capture(oneOrMore("0" - "9")) ~ optional(WhiteSpace) ~> (_.toInt) }

  def NormalChar = rule { !anyOf("\"\\") ~ ANY }

  def WhiteSpace = rule { zeroOrMore(WhiteSpaceChar) }

  val WhiteSpaceChar = CharPredicate(" \n\r\t\f")

}