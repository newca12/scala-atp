package org.edla.port.atp

import org.edla.port.atp.Intro.{Add, Const, Expression, Mul, Var, parseExpression, printExp, simplify}
import org.edla.port.atp.Lex.lex
import org.scalatest.FunSuite

class IntroSpec extends FunSuite {
  val exp: Expression = Add(Mul(Add(Mul(Const(0), Var("x")), Const(1)), Const(3)), Const(12))

  test("intro.p002") {
    assert(simplify(exp) === Const(15))
  }
  test("intro.p003") {
    val input = "2*((var_1 + x') + 11)"
    assert(lex(input).mkString(";") === "2;*;(;(;var_1;+;x;';);+;11;)")
  }
  test("intro.p004") {
    val input = "if (*p1-- == *p2++) then f() else g()"
    assert(lex(input).mkString(";") === "if;(;*;p1;--;==;*;p2;++;);then;f;(;);else;g;(;)")
  }
  test("intro.p005") {
    val input = "x + 1"
    assert(parseExpression(input).toString === "Add(Var(x),Const(1))")
  }
  test("intro.p006") {
    val input = "(x1 + x2 + x3) * (1 + 2 + 3 * x + y)"
    assert(
      parseExpression(input).toString === "Mul(Add(Var(x1),Add(Var(x2),Var(x3))),Add(Const(1),Add(Const(2),Add(Mul(Const(3),Var(x)),Var(y)))))"
    )
  }
  test("intro.p008") {
    val input = "x + 3 * y"
    assert(printExp(parseExpression(input)) === input)
  }
  test("intro.p009") {
    val input = "(x + 3) * y"
    assert(printExp(parseExpression(input)) === input)
  }
  test("intro.p010") {
    val input = "1 + 2 + 3"
    assert(printExp(parseExpression(input)) === input)
  }
  test("intro.p011") {
    val input = "((1 + 2) + 3) + 4"
    assert(printExp(parseExpression(input)) === input)
  }
  test("intro.p012") {
    val input =
      "(x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10) * (y1 + y2 + y3 + y4 + y5 + y6 + y7 + y8 + y9 + y10)"
    assert(printExp(parseExpression(input)) === input)
  }

}
