package org.edla.study.parsing.parboiled

import org.parboiled.scala._

class OrderDsl extends Parser {

  
  
  def order = rule { items ~ WhiteSpace ~ account_spec }

  def items = rule { "(" ~ oneOrMore(line_item, separator = ", ") ~ ")" }

  def line_item = rule { security_spec ~ WhiteSpace ~ buy_sell ~ WhiteSpace ~ price_spec }

  def buy_sell = rule { "to" ~ WhiteSpace ~ ("buy" | "sell") }
  
  def security_spec = rule { numericLit ~ WhiteSpace ~ (ident ~ WhiteSpace ~ "shares" ) }
  
  def price_spec = rule { "at" ~ WhiteSpace ~ optional(min_max) ~ WhiteSpace ~ numericLit}
  
  def min_max = rule { "min" | "max"}
  
  def account_spec = rule { "for" ~ WhiteSpace ~ "account" ~ WhiteSpace ~ stringLit ~ WhiteSpace }

  def stringLit = rule { "\"" ~ zeroOrMore(NormalChar) ~ "\" " }
  
  def ident = rule { oneOrMore("a" - "z" | "A" - "Z")  }
  
  def numericLit = rule { oneOrMore("0" - "9") }
  
  def NormalChar = rule { !anyOf("\"\\") ~ ANY }
  
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