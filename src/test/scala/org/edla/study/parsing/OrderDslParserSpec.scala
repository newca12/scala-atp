package org.edla.study.parsing

import _root_.fastparse.Parsed
import _root_.fastparse._
import org.edla.study.parsing.ch8.trading.semantic.OrderDsl
import org.edla.study.parsing.common.AST._
import org.edla.study.parsing.fastparse.semantic.{OrderDsl => OrderDslFastParse}
import org.edla.study.parsing.parboiled.semantic.{OrderDsl => OrderDslParboiled}
import org.scalatest.FunSuite

import scala.util.Success

class OrderDslParserSpec extends FunSuite {

  val accountSample = """for account "A1234""""

  val accountAST = AccountSpec("A1234")

  val orderSample = ("""(100 IBM shares to buy at max 45, 40 Sun shares
      to sell at min 24, 25 CISCO shares to buy at max 56) 
      for account "A1234"""")

  val orderAST = Order(
    Items(
      List(
        LineItem(SecuritySpec(100, "IBM"), BUY, PriceSpec(Some(MAX), 45)),
        LineItem(
          SecuritySpec(40, "Sun"),
          SELL,
          PriceSpec(Some(MIN), 24)
        ),
        LineItem(
          SecuritySpec(25, "CISCO"),
          BUY,
          PriceSpec(Some(MAX), 56)
        )
      )
    ),
    AccountSpec("A1234")
  )

  test("Scala Standard Parser Combinator AccountSpec Parser") {
    assert(OrderDsl.parse(OrderDsl.account_spec, accountSample) === accountAST)
  }

  test("Scala Standard Parser Combinator Order Parser") {
    assert(OrderDsl.parse(OrderDsl.order, orderSample) === orderAST)
  }

  test("FastParse AccountSpec Parser") {
    val Parsed.Success(value, _) = parse(accountSample, OrderDslFastParse.account_spec(_))
    assert(value === accountAST)
  }

  test("FastParse Order Parser") {
    val Parsed.Success(value, _) = parse(orderSample, OrderDslFastParse.order(_))
    assert(value === orderAST)
  }

  test("Parboiled AccountSpec Parser") {
    assert(OrderDslParboiled(accountSample).account_spec.run() === Success(accountAST))
  }

  test("Parboiled Order Parser") {
    assert(OrderDslParboiled(orderSample).order.run() === Success(orderAST))
  }

}
