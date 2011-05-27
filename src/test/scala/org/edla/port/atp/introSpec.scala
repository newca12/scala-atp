package org.edla.port.atp

import org.specs2.mutable._
import org.edla.port.atp.Intro._

class IntroSpec extends SpecificationWithJUnit {
  val exp: Expression = Add(Mul(Add(Mul(Const(0), Var("x")), Const(1)), Const(3)), Const(12))
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
    "be 2;*;(;(;var_1;+;x;';);+;11;)" in {
      val input: String = "if (*p1-- == *p2++) then f() else g()"
      lex(input).mkString(";") must equalTo("if;(;*;p1;--;==;*;p2;++;);then;f;(;);else;g;(;)")
    }
  }
}