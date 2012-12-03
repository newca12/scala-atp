package org.edla.port.atp

import org.edla.port.atp.Intro.Add
import org.edla.port.atp.Intro.Const
import org.edla.port.atp.Intro.Expression
import org.edla.port.atp.Intro.Mul
import org.edla.port.atp.Intro.Var
import org.edla.port.atp.Lex.lex
import org.edla.port.atp.Intro.parseExpression
import org.edla.port.atp.Intro.simplify
import org.edla.port.atp.Intro.printExp
import org.junit.runner.RunWith
import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class IntroSpec extends SpecificationWithJUnit {
  val exp: Expression = Add(Mul(Add(Mul(Const(0), Var("x")), Const(1)), Const(3)), Const(12))
  //sequential
  "Simplified Expression should be Const(15)" in {
    simplify(exp) must equalTo(Const(15))
  }
  "Explode input should be 2;*;(;(;var_1;+;x;';);+;11;)" in {
    val input = "2*((var_1 + x') + 11)"
    lex(input).mkString(";") must equalTo("2;*;(;(;var_1;+;x;';);+;11;)")
  }
  "Explode input be if;(;*;p1;--;==;*;p2;++;);then;f;(;);else;g;(;)" in {
    val input = "if (*p1-- == *p2++) then f() else g()"
    lex(input).mkString(";") must equalTo("if;(;*;p1;--;==;*;p2;++;);then;f;(;);else;g;(;)")
  }
  "Parse input should be Add(Var(x),Const(1))" in {
    val input = "x + 1"
    parseExpression(input).toString must equalTo("Add(Var(x),Const(1))")
  }
  "Parse input should be Mul(Add(Var(x1),Add(Var(x2),Var(x3))),Add(Const(1),Add(Const(2),Add(Mul(Const(3),Var(x)),Var(y)))))" in {
    val input = "(x1 + x2 + x3) * (1 + 2 + 3 * x + y)"
    parseExpression(input).toString must equalTo("Mul(Add(Var(x1),Add(Var(x2),Var(x3))),Add(Const(1),Add(Const(2),Add(Mul(Const(3),Var(x)),Var(y)))))")
  }
  "Pretty print input should be x + 3 * y" in {
    val input = "x + 3 * y"
    printExp(parseExpression(input)) must equalTo(input)
  }
  "Pretty print input should be (x + 3) * y" in {
    val input = "(x + 3) * y"
    printExp(parseExpression(input)) must equalTo(input)
  }
  "Pretty print input should be 1 + 2 + 3" in {
    val input = "1 + 2 + 3"
    printExp(parseExpression(input)) must equalTo(input)
  }
  "Pretty print input should be ((1 + 2) + 3) + 4" in {
    val input = "((1 + 2) + 3) + 4"
    printExp(parseExpression(input)) must equalTo(input)
  }
  "Pretty print input should be (x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10) * (y1 + y2 + y3 + y4 + y5 + y6 + y7 + y8 + y9 + y10)" in {
    val input = "(x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10) * (y1 + y2 + y3 + y4 + y5 + y6 + y7 + y8 + y9 + y10)"
    printExp(parseExpression(input)) must equalTo(input)
  }

}
