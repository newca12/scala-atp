package org.edla.port.atp

import org.specs2.mutable._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import org.edla.port.atp.Intro._

@RunWith(classOf[JUnitRunner])
class IntroSpec extends SpecificationWithJUnit {
  val exp: Expression = Add(Mul(Add(Mul(Const(0), Var("x")), Const(1)), Const(3)), Const(12))
  //sequential
  "Simplified Expression" should {
    "be Const(15)" in {
      simplify(exp) must equalTo(Const(15))
    }
  }
  "Explode input" should {
    "be 2;*;(;(;var_1;+;x;';);+;11;)" in {
      val input: String = "2*((var_1 + x') + 11)"
      lex(input).mkString(";") must equalTo("2;*;(;(;var_1;+;x;';);+;11;)")
    }
  }
  "Explode input" should {
    "be if;(;*;p1;--;==;*;p2;++;);then;f;(;);else;g;(;)" in {
      val input: String = "if (*p1-- == *p2++) then f() else g()"
      lex(input).mkString(";") must equalTo("if;(;*;p1;--;==;*;p2;++;);then;f;(;);else;g;(;)")
    }
  }
  "Parse input" should {
    "be Add(Var(x),Const(1))" in {
      val input: String = "x + 1"
      parseExpression(input).toString must equalTo("Add(Var(x),Const(1))")
    }
  }
  "Parse input" should {
    "be Mul(Add(Var(x1),Add(Var(x2),Var(x3))),Add(Const(1),Add(Const(2),Add(Mul(Const(3),Var(x)),Var(y)))))" in {
      val input: String = "(x1 + x2 + x3) * (1 + 2 + 3 * x + y)"
      parseExpression(input).toString must equalTo("Mul(Add(Var(x1),Add(Var(x2),Var(x3))),Add(Const(1),Add(Const(2),Add(Mul(Const(3),Var(x)),Var(y)))))")
    }
  }
  "Pretty print input" should {
    "be x + 3 * y" in {
      val input: String = "x + 3 * y"
      stringOfExp(0, parseExpression(input)) must equalTo("x + 3 * y")
    }
  }
  "Pretty print input" should {
    "be (x + 3) * y" in {
      val input: String = "(x + 3) * y"
      stringOfExp(0, parseExpression(input)) must equalTo("(x + 3) * y")
    }
  }
  "Pretty print input" should {
    "be 1 + 2 + 3" in {
      val input: String = "1 + 2 + 3"
      stringOfExp(0, parseExpression(input)) must equalTo("1 + 2 + 3")
    }
  }
  "Pretty print input" should {
    "be ((1 + 2) + 3) + 4" in {
      val input: String = "((1 + 2) + 3) + 4"
      stringOfExp(0, parseExpression(input)) must equalTo("((1 + 2) + 3) + 4")
    }
  }
}