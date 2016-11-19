// Listing 8.6 AST for Order Processing DSL

package org.edla.study.parsing.ch8.trading.semantic

import scala.language.postfixOps
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

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

object OrderDsl extends StandardTokenParsers {

  def parse[T](parser: Parser[T], input: String): T =
    phrase(parser)(new lexical.Scanner(input)) match {
      case Success(ast, _) ⇒ ast
      case e: NoSuccess    ⇒ sys.error("parser error: " + e.msg)
    }

  lexical.reserved +=
    ("to", "buy", "sell", "min", "max", "for", "account", "shares", "at")

  lexical.delimiters += ("(", ")", ",")

  lazy val order: Parser[Order] =
    items ~ account_spec ^^ { case i ~ a ⇒ Order(i, a) }

  lazy val items: Parser[Items] =
    "(" ~> rep1sep(line_item, ",") <~ ")" ^^ Items

  lazy val line_item: Parser[LineItem] =
    security_spec ~ buy_sell ~ price_spec ^^ { case s ~ b ~ p ⇒ LineItem(s, b, p) }

  lazy val buy_sell: Parser[BuySell] =
    "to" ~> ("buy" ^^^ BUY | "sell" ^^^ SELL)

  lazy val security_spec: Parser[SecuritySpec] =
    numericLit ~ (ident <~ "shares") ^^ { case n ~ s ⇒ SecuritySpec(n.toInt, s) }

  lazy val price_spec: Parser[PriceSpec] =
    "at" ~> (min_max ?) ~ numericLit ^?
      ({ case m ~ p if p.toInt > 20 ⇒ PriceSpec(m, p.toInt) },
      (m ⇒ "price needs to be > 20"))

  lazy val min_max: Parser[PriceType] =
    "min" ^^^ MIN | "max" ^^^ MAX

  lazy val account_spec: Parser[AccountSpec] =
    "for" ~> "account" ~> stringLit ^^ AccountSpec
}
