package org.edla.port.atp

import org.edla.port.atp.Formulas.Formula
import org.edla.port.atp.Prop._
import org.edla.port.atp.PropExamples.ramsey
import org.scalatest.FunSuite

class PropExamplesSpec extends FunSuite {

  implicit def default_parser(s: String): Formula = parse_prop_formula(s)


  test("prop.p002") {
    assert(tautology(ramsey(3,3,5)) === false
    )
  }

  test("prop.p003") {
    assert(tautology(ramsey(3,3,6)) === true
    )
  }

}
