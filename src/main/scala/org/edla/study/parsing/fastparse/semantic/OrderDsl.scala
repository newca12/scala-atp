package org.edla.study.parsing.fastparse.semantic

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
import fastparse._, MultiLineWhitespace._

object OrderDsl {

  /*
  val White = WhitespaceApi.Wrapper {
    import fastparse.all._
    NoTrace(P(CharsWhile(" \n".contains(_)).?))
  }
  import fastparse.noApi._
  import White._
   */

  def digits[_: P]        = P(CharsWhile('0' to '9' contains (_)))
  def hexDigit[_: P]      = P(CharIn("0-9", "a-f", "A-F"))
  def unicodeEscape[_: P] = P("u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit)
  def escape[_: P]        = P("\\" ~ (CharIn("\"/\\bfnrt") | unicodeEscape))
  def strChars[_: P]      = P(CharsWhile(!"\"\\".contains(_)))
  def stringLit[_: P]     = P("\"" ~/ (strChars | escape).rep.! ~ "\"")
  def ident[_: P]         = P(CharsWhile(('a' to 'z') ++ ('A' to 'Z') contains (_)))

  def order[_: P]: P[Order] = P(items ~ account_spec).map { case (a: Items, b: AccountSpec) => Order(a, b) }

  def items[_: P]: P[Items] = P("(" ~ line_item.rep(sep = ",") ~ ")").map { case (a: Seq[LineItem]) => Items(a.toList) }

  def line_item[_: P]: P[LineItem] = P(security_spec ~ buy_sell ~ price_spec).map {
    case (a: SecuritySpec, b: BuySell, c: PriceSpec) => LineItem(a, b, c)
  }

  def buy_sell[_: P]: P[BuySell] = P("to" ~ ("buy".! | "sell".!)).map {
    case "buy"  => BUY
    case "sell" => SELL
  }

  def security_spec[_: P]: P[SecuritySpec] = P(digits.! ~ ident.! ~ "shares").map {
    case (a, b) => SecuritySpec(a.toInt, b)
  }

  def price_spec[_: P]: P[PriceSpec] = P("at" ~ min_max.? ~ digits.!).map {
    case (a: Option[PriceType], b: String) => PriceSpec(a, b.toInt)
  }

  def min_max[_: P]: P[PriceType] = P(("min" | "max").!).map {
    case "min" => MIN
    case "max" => MAX
  }

  def account_spec[_: P]: P[AccountSpec] = P("for" ~ "account" ~ stringLit).map(AccountSpec)

}
