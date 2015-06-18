package org.edla.study.parsing.fastparse.semantic

import org.edla.study.parsing.common.AST.AccountSpec
import org.edla.study.parsing.common.AST.BUY
import org.edla.study.parsing.common.AST.BuySell
import org.edla.study.parsing.common.AST.LineItem
import org.edla.study.parsing.common.AST.MAX
import org.edla.study.parsing.common.AST.MIN
import org.edla.study.parsing.common.AST.PriceSpec
import org.edla.study.parsing.common.AST.PriceType
import org.edla.study.parsing.common.AST.SELL
import org.edla.study.parsing.common.AST.SecuritySpec
import fastparse._

object OrderDsl {

  val space = P(CharsWhile(" \n".contains(_)).?)
  val digits = P(CharsWhile('0' to '9' contains (_)))
  val hexDigit = P(CharIn('0' to '9', 'a' to 'f', 'A' to 'F'))
  val unicodeEscape = P("u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit)
  val escape = P("\\" ~ (CharIn("\"/\\bfnrt") | unicodeEscape))
  val strChars = P(CharsWhile(!"\"\\".contains(_)))
  val stringLit = P(space ~ "\"" ~! (strChars | escape).rep.! ~ "\"")
  val ident = P(CharsWhile(('a' to 'z') ++ ('A' to 'Z') contains (_)))

  val line_item = P(security_spec ~ space ~ buy_sell ~ space ~ price_spec).map {
    case (a: SecuritySpec, b: BuySell, c: PriceSpec) ⇒ LineItem(a, b, c)
  }

  val buy_sell: Parser[BuySell] = P("to" ~ space ~ ("buy".! | "sell".!)).map {
    case "buy"  ⇒ BUY
    case "sell" ⇒ SELL
  }

  val security_spec: Parser[SecuritySpec] = P(digits.! ~ space ~ ident.! ~ space ~ "shares").map {
    case (a, b) ⇒ SecuritySpec(a.toInt, b)
  }

  val price_spec = P("at" ~ space ~ min_max.? ~ space ~ digits.!).map {
    case (a: Option[PriceType], b: String) ⇒ PriceSpec(a, b.toInt)
  }

  val min_max: Parser[PriceType] = P(("min" | "max").!).map {
    case "min" ⇒ MIN
    case "max" ⇒ MAX
  }

}