package org.edla.port.atp

import org.edla.port.atp.Formulas.And
import org.edla.port.atp.Formulas.Atom
import org.edla.port.atp.Formulas.Iff
import org.edla.port.atp.Formulas.Imp
import org.edla.port.atp.Formulas.Not
import org.edla.port.atp.Formulas.Or
import org.edla.port.atp.Formulas.True
import org.edla.port.atp.Prop.atoms
import org.edla.port.atp.Prop.dual
import org.edla.port.atp.Prop.eval
import org.edla.port.atp.Prop.nnf
import org.edla.port.atp.Prop.parse_prop_formula
import org.edla.port.atp.Prop.psimplify
import org.edla.port.atp.Prop.purednf
import org.edla.port.atp.Prop.rawdnf
import org.edla.port.atp.Prop.tautology
import org.edla.port.atp.Prop.trivial
import org.junit.runner.RunWith
import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PropSpec extends SpecificationWithJUnit {

  implicit def default_parser(s: String) = parse_prop_formula(s)

  """correctly parse and print formulas""" in {
    parse_prop_formula("""p""") must equalTo(Atom("p"))
    parse_prop_formula("""true""") must equalTo(True())
    val fm = parse_prop_formula("""p ==> q <=> r /\ s \/ (t <=> ~(~u) /\ v)""")
    fm must equalTo(
      Iff(Imp(Atom("p"), Atom("q")), Or(And(Atom("r"), Atom("s")), Iff(Atom("t"), And(Not(Not(Atom("u"))), Atom("v"))))))
    fm.toString must equalTo("""p ==> q <=> r /\ s \/ t <=> ~~u /\ v""")
  }

  """correctly evaluate formulas""" in {
    eval("""p /\ q ==> q /\ r""")(_ match {
      case "p" ⇒ true
      case "q" ⇒ false
      case "r" ⇒ true
    }) must equalTo(true)

    eval("""p /\ q ==> q /\ r"""")(_ match {
      case "p" ⇒ true
      case "q" ⇒ true
      case "r" ⇒ false
    }) must equalTo(false)
  }

  """correctly enumerate atoms""" in {
    atoms("""p /\ q \/ s ==> ~p \/ (r <=> s)""") must equalTo(List("p", "q", "r", "s"))
  }

  """correctly compute tautology""" in {
    tautology("""p \/ ~p""") must equalTo(true)
    tautology("""p \/ q ==> p""") must equalTo(false)
    tautology("""p \/ q ==> q \/ (p <=> q)""") must equalTo(false)
    tautology("""(p \/ q) /\ ~(p /\ q) ==> (~p <=> q)""") must equalTo(true)
  }

  """correctly compute dual""" in {
    dual("""p \/ ~p""") must equalTo(parse_prop_formula("""p /\ ~p"""))
  }

  """correctly simplify formulas""" in {
    psimplify("""(true ==> (x <=> false)) ==> ~(y \/ false /\ z)""") must equalTo(parse_prop_formula("""~x ==> ~y"""))
    psimplify("""((x ==> y) ==> true) \/ ~false""") must equalTo(parse_prop_formula("true"))
  }

  """correctly transform formula in negation normal form""" in {
    tautology(Iff("""(p <=> q) <=> ~(r ==> s)""", nnf("""(p <=> q) <=> ~(r ==> s)"""))) must equalTo(true)
  }

  """correctly transform formula in raw dnf""" in {
    tautology(Iff(rawdnf("""(p \/ q /\ r) /\ (~p \/ ~r)"""),
      """(p /\ ~p \/ (q /\ r) /\ ~p) \/ p /\ ~r \/ (q /\ r) /\ ~r""")) must equalTo(true)
  }

  """correctly transform formula in negation normal form""" in {
    purednf("""(p \/ q /\ r) /\ (~p \/ ~r)""").filter(!trivial(_)) must equalTo(
      List(List(Atom("p"), Not(Atom("r"))), List(Atom("q"), Atom("r"), Not(Atom("p")))))
  }
}