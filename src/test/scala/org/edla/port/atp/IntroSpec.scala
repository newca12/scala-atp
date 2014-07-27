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
  "intro.p002" in {
    simplify(exp) must equalTo(Const(15))
  }
  "intro.p003" in {
    val input = "2*((var_1 + x') + 11)"
    lex(input).mkString(";") must equalTo("2;*;(;(;var_1;+;x;';);+;11;)")
  }
  "intro.p004" in {
    val input = "if (*p1-- == *p2++) then f() else g()"
    lex(input).mkString(";") must equalTo("if;(;*;p1;--;==;*;p2;++;);then;f;(;);else;g;(;)")
  }
  "intro.p005" in {
    val input = "x + 1"
    parseExpression(input).toString must equalTo("Add(Var(x),Const(1))")
  }
  "intro.p006" in {
    val input = "(x1 + x2 + x3) * (1 + 2 + 3 * x + y)"
    parseExpression(input).toString must equalTo("Mul(Add(Var(x1),Add(Var(x2),Var(x3))),Add(Const(1),Add(Const(2),Add(Mul(Const(3),Var(x)),Var(y)))))")
  }
  "intro.p008" in {
    val input = "x + 3 * y"
    printExp(parseExpression(input)) must equalTo(input)
  }
  "intro.p009" in {
    val input = "(x + 3) * y"
    printExp(parseExpression(input)) must equalTo(input)
  }
  "intro.p010" in {
    val input = "1 + 2 + 3"
    printExp(parseExpression(input)) must equalTo(input)
  }
  "intro.p011" in {
    val input = "((1 + 2) + 3) + 4"
    printExp(parseExpression(input)) must equalTo(input)
  }
  "intro.p012" in {
    val input = "(x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10) * (y1 + y2 + y3 + y4 + y5 + y6 + y7 + y8 + y9 + y10)"
    printExp(parseExpression(input)) must equalTo(input)
  }

}
