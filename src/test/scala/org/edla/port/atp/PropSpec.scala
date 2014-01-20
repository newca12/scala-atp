package org.edla.port.atp

import org.edla.port.atp.Prop.parse_prop_formula
import org.edla.port.atp.Prop.tautology
import org.junit.runner.RunWith
import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PropSpec extends SpecificationWithJUnit {

  implicit def default_parser(s: String) = parse_prop_formula(s)

  """Tautology p \/ ~p should be true""" in {
    tautology("""p \/ ~p""") must equalTo(true)
  }

  """Tautology p \/ q ==> p should be false""" in {
    tautology("""p \/ q ==> p""") must equalTo(false)
  }

  """Tautology p \/ q ==> q \/ (p <=> q) should be false""" in {
    tautology("""p \/ q ==> q \/ (p <=> q)""") must equalTo(false)
  }

  "Tautology (p ∨ q) ∧ ¬(p ∧ q) ⇒ (¬p ⇔ q) should be true" in {
    tautology("""(p \/ q) /\ ~(p /\ q) ==> (~p <=> q)""") must equalTo(true)
  }

}