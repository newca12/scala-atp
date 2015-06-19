package org.edla.study.parsing

import scala.util.{ Failure, Success }

import org.edla.study.parsing.common.AST.{ AccountSpec, BUY, Items, LineItem, MAX, MIN, Order, PriceSpec, SELL, SecuritySpec }
import org.edla.study.parsing.parboiled.semantic.{ OrderDsl ⇒ OrderDslParboiled }
import org.edla.study.parsing.fastparse.semantic.{ OrderDsl ⇒ OrderDslFastParse }
import org.parboiled2.{ ErrorFormatter, ParseError }
import org.parboiled2.ParserInput.apply
import fastparse._
import _root_.fastparse._
import _root_.fastparse.core.Result
import Result.{ Success ⇒ FastParseSuccess, Failure ⇒ FastParseFailure }
import org.scalatest.{ Finders, FunSuite }

class OrderDslParserSpec extends FunSuite {

  val accountSample = """for account "A1234""""

  val accountAST = AccountSpec("A1234")

  val orderSample = ("""(100 IBM shares to buy at max 45, 40 Sun shares
      to sell at min 24, 25 CISCO shares to buy at max 56) 
      for account "A1234"""")

  val orderAST = Order(
    Items(List(LineItem(SecuritySpec(100, "IBM"), BUY, PriceSpec(Some(MAX), 45)), LineItem(
      SecuritySpec(40, "Sun"), SELL, PriceSpec(Some(MIN), 24)
    ), LineItem(
      SecuritySpec(25, "CISCO"), BUY, PriceSpec(Some(MAX), 56)
    ))), AccountSpec("A1234")
  )

  test("FastParse AccountSpec Parser") {
    val Result.Success(value, index) = OrderDslFastParse.account_spec.parse(accountSample)
    assert(value === accountAST)
  }

  test("FastParse Order Parser") {
    val Result.Success(value, index) = OrderDslFastParse.order.parse(orderSample)
    assert(value === orderAST)
  }

  test("Parboiled AccountSpec Parser") {
    assert(parseAccountSpec(accountSample) === accountAST)
  }

  test("Parboiled Order Parser") {
    assert(parseOrder(orderSample) === orderAST)

  }

  def parseAccountSpec(s: String): AccountSpec = {
    OrderDslParboiled.account_spec.run(s) match {
      case Success(result)        ⇒ result
      case Failure(e: ParseError) ⇒ sys.error(e.format(s, new ErrorFormatter(showTraces = true)))
      case Failure(e)             ⇒ throw e
    }
  }

  def parseOrder(s: String): Order = {
    OrderDslParboiled.order.run(s) match {
      case Success(result)        ⇒ result
      case Failure(e: ParseError) ⇒ sys.error(e.format(s, new ErrorFormatter(showTraces = true)))
      case Failure(e)             ⇒ throw e
    }
  }

}
