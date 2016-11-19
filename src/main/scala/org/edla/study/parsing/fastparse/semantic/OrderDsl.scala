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
import fastparse._

object OrderDsl {

  val White = WhitespaceApi.Wrapper {
    import fastparse.all._
    NoTrace(P(CharsWhile(" \n".contains(_)).?))
  }
  import fastparse.noApi._
  import White._

  val digits        = P(CharsWhile('0' to '9' contains (_)))
  val hexDigit      = P(CharIn('0' to '9', 'a' to 'f', 'A' to 'F'))
  val unicodeEscape = P("u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit)
  val escape        = P("\\" ~ (CharIn("\"/\\bfnrt") | unicodeEscape))
  val strChars      = P(CharsWhile(!"\"\\".contains(_)))
  val stringLit     = P("\"" ~/ (strChars | escape).rep.! ~ "\"")
  val ident         = P(CharsWhile(('a' to 'z') ++ ('A' to 'Z') contains (_)))

  val order: Parser[Order] = P(items ~ account_spec).map { case (a: Items, b: AccountSpec) ⇒ Order(a, b) }

  val items: Parser[Items] = P("(" ~ line_item.rep(sep = ",") ~ ")").map { case (a: Seq[LineItem]) ⇒ Items(a.toList) }

  val line_item: Parser[LineItem] = P(security_spec ~ buy_sell ~ price_spec).map {
    case (a: SecuritySpec, b: BuySell, c: PriceSpec) ⇒ LineItem(a, b, c)
  }

  val buy_sell: Parser[BuySell] = P("to" ~ ("buy".! | "sell".!)).map {
    case "buy"  ⇒ BUY
    case "sell" ⇒ SELL
  }

  val security_spec: Parser[SecuritySpec] = P(digits.! ~ ident.! ~ "shares").map {
    case (a, b) ⇒ SecuritySpec(a.toInt, b)
  }

  val price_spec: Parser[PriceSpec] = P("at" ~ min_max.? ~ digits.!).map {
    case (a: Option[PriceType], b: String) ⇒ PriceSpec(a, b.toInt)
  }

  val min_max: Parser[PriceType] = P(("min" | "max").!).map {
    case "min" ⇒ MIN
    case "max" ⇒ MAX
  }

  val account_spec: Parser[AccountSpec] = P("for" ~ "account" ~ stringLit).map(AccountSpec)

}
