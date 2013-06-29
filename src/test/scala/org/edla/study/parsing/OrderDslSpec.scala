package org.edla.study.parsing

import org.edla.study.parsing.common.AST.AccountSpec
import org.edla.study.parsing.common.AST.BUY
import org.edla.study.parsing.common.AST.Items
import org.edla.study.parsing.common.AST.LineItem
import org.edla.study.parsing.common.AST.MAX
import org.edla.study.parsing.common.AST.MIN
import org.edla.study.parsing.common.AST.Order
import org.edla.study.parsing.common.AST.PriceSpec
import org.edla.study.parsing.common.AST.SELL
import org.edla.study.parsing.common.AST.SecuritySpec
import org.edla.study.parsing.parboiled.semantic.OrderDsl
import org.parboiled.errors.ErrorUtils
import org.parboiled.scala.ParsingResult.unwrap
import org.parboiled.scala.ReportingParseRunner
import org.parboiled.scala.Rule0
import org.parboiled.scala.Rule1
import org.parboiled.scala.string2Input
import org.scalatest.FunSpec
import org.scalatest.matchers.MatchResult
import org.scalatest.matchers.Matcher
import org.scalatest.matchers.ShouldMatchers

import OrderDslSpec.account_spec
import OrderDslSpec.order

object OrderDslSpec extends OrderDsl {
  case class Parsing(s: String) {
    def parsed_with[T](r: Rule1[T]) = ParseCommand(r, s)
    def parsed_with(r: Rule0) = ParseCommand0(r, s)

  }

  implicit def str2Parse(s: String) = Parsing(s)

  case class ParseCommand[T](r: Rule1[T], s: String) {
    def run = {
      val runner = ReportingParseRunner(r)
      runner.run(s)
    }
  }
  case class ParseCommand0(r: Rule0, s: String)

  class ParseMatcher[T](a: T) extends Matcher[ParseCommand[_ >: T]] {

    def apply(c: ParseCommand[_ >: T]) = {

      val res = c.run
      val b = res.result

      val failureMessageSuffix = (for (r <- b) yield "'" + c.s + "' resulted in " + r + " which did not equal " + a).
        getOrElse("failed: " + ErrorUtils.printParseErrors(res))

      val negatedFailureMessageSuffix =
        "'" + c.s + "' yielded " + a

      val equals = (for (r <- b) yield r == a).getOrElse(false)

      MatchResult(
        equals,
        "Parsing " + failureMessageSuffix,
        "Parsing " + negatedFailureMessageSuffix,
        "Parsing " + failureMessageSuffix,
        "Parsing " + negatedFailureMessageSuffix)
    }
  }

  def create[T](a: T) = new ParseMatcher(a)

}

class OrderDslSpec extends FunSpec with ShouldMatchers {

  import OrderDslSpec._

  describe("A PEG OrderDsl Parser") {
    it("Should accept account spec") {
      """for account "A1234"""" parsed_with account_spec should create(AccountSpec("A1234"))
    }

    it("Should accept order") {
      """(100 IBM shares to buy at max 45, 40 Sun shares 
      to sell at min 24, 25 CISCO shares to buy at max 56) 
      for account "A1234"""" parsed_with order should create(Order(
        Items(List(LineItem(SecuritySpec(100, "IBM"), BUY, PriceSpec(Some(MAX), 45)), LineItem(
          SecuritySpec(40, "Sun"), SELL, PriceSpec(Some(MIN), 24)), LineItem(
          SecuritySpec(25, "CISCO"), BUY, PriceSpec(Some(MAX), 56)))), AccountSpec("A1234")))
    }
  }

}