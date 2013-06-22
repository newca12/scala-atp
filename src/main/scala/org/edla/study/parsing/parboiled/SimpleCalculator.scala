package org.edla.study.parsing.parboiled

import org.parboiled.scala._

class SimpleCalculator extends Parser {

  def Expression: Rule0 = rule { Term ~ zeroOrMore(anyOf("+-") ~ Term) }

  def Term = rule { Factor ~ zeroOrMore(anyOf("*/") ~ Factor) }

  def Factor = rule { Number | "(" ~ Expression ~ ")" }

  def Number = rule { oneOrMore("0" - "9") }
}