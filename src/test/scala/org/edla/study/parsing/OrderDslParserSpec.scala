package org.edla.study.parsing

import scala.util.{ Failure, Success }
import org.specs2.mutable.Specification
import org.parboiled2._
import org.edla.study.parsing.parboiled.semantic.OrderDsl

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

import org.junit.runner.RunWith
import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class OrderDslParserSpec extends SpecificationWithJUnit {

  "The OrderDslParser" should {

    "A PEG OrderDsl Parser" in (
      parseAccountSpec("""for account "A1234"""") ===
      AccountSpec("A1234"))

    "Should accept order" in (
      parseOrder("""(100 IBM shares to buy at max 45, 40 Sun shares 
      to sell at min 24, 25 CISCO shares to buy at max 56) 
      for account "A1234"""") ===
      Order(
        Items(List(LineItem(SecuritySpec(100, "IBM"), BUY, PriceSpec(Some(MAX), 45)), LineItem(
          SecuritySpec(40, "Sun"), SELL, PriceSpec(Some(MIN), 24)), LineItem(
          SecuritySpec(25, "CISCO"), BUY, PriceSpec(Some(MAX), 56)))), AccountSpec("A1234")))
  }

  def parseAccountSpec(s: String): AccountSpec = {
    val parser = new OrderDsl(s)
    parser.account_spec.run() match {
      case Success(result)        ⇒ result
      case Failure(e: ParseError) ⇒ sys.error(parser.formatError(e, showTraces = true))
      case Failure(e)             ⇒ throw e
    }
  }

  def parseOrder(s: String): Order = {
    val parser = new OrderDsl(s)
    parser.order.run() match {
      case Success(result)        ⇒ result
      case Failure(e: ParseError) ⇒ sys.error(parser.formatError(e, showTraces = true))
      case Failure(e)             ⇒ throw e
    }
  }
}