package org.edla.study.parsing

import scala.util.{ Failure, Success }

import org.edla.study.parsing.common.AST.{ AccountSpec, BUY, Items, LineItem, MAX, MIN, Order, PriceSpec, SELL, SecuritySpec }
import org.edla.study.parsing.parboiled.semantic.OrderDsl
import org.parboiled2.{ ErrorFormatter, ParseError }
import org.parboiled2.ParserInput.apply
import org.scalatest.{ Finders, FunSuite }

class OrderDslParserSpec extends FunSuite {

  test("A PEG OrderDsl Parser") {
    assert(parseAccountSpec("""for account "A1234"""") ===
      AccountSpec("A1234"))
  }

  test("Should accept order") {
    assert(parseOrder("""(100 IBM shares to buy at max 45, 40 Sun shares
      to sell at min 24, 25 CISCO shares to buy at max 56) 
      for account "A1234"""") ===
      Order(
        Items(List(LineItem(SecuritySpec(100, "IBM"), BUY, PriceSpec(Some(MAX), 45)), LineItem(
          SecuritySpec(40, "Sun"), SELL, PriceSpec(Some(MIN), 24)
        ), LineItem(
          SecuritySpec(25, "CISCO"), BUY, PriceSpec(Some(MAX), 56)
        ))), AccountSpec("A1234")
      ))
  }

  def parseAccountSpec(s: String): AccountSpec = {
    OrderDsl.account_spec.run(s) match {
      case Success(result)        ⇒ result
      case Failure(e: ParseError) ⇒ sys.error(e.format(s, new ErrorFormatter(showTraces = true)))
      case Failure(e)             ⇒ throw e
    }
  }

  def parseOrder(s: String): Order = {
    OrderDsl.order.run(s) match {
      case Success(result)        ⇒ result
      case Failure(e: ParseError) ⇒ sys.error(e.format(s, new ErrorFormatter(showTraces = true)))
      case Failure(e)             ⇒ throw e
    }
  }
}