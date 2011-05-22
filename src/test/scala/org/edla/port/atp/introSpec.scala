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
}