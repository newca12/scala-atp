package org.edla.port.atp

import org.edla.port.atp.Formulas.And
import org.edla.port.atp.Formulas.Atom
import org.edla.port.atp.Formulas.Iff
import org.edla.port.atp.Formulas.Imp
import org.edla.port.atp.Formulas.Not
import org.edla.port.atp.Formulas.Or
import org.edla.port.atp.Formulas.True
import org.edla.port.atp.Formulas.print_formula
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
//needed
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PropSpec extends SpecificationWithJUnit {

  implicit def default_parser(s: String) = parse_prop_formula(s)

  "prop.p001" in {
    val fm = parse_prop_formula("""p ==> q <=> r /\ s \/ (t <=> ~ ~u /\ v)""")
    val f = And(fm, fm)
    f must equalTo(
      And(
        Iff(Imp(Atom("p"), Atom("q")), Or(And(Atom("r"), Atom("s")), Iff(Atom("t"), And(Not(Not(Atom("u"))), Atom("v"))))),
        Iff(Imp(Atom("p"), Atom("q")), Or(And(Atom("r"), Atom("s")), Iff(Atom("t"), And(Not(Not(Atom("u"))), Atom("v")))))))
    val stream = new java.io.ByteArrayOutputStream()
    Console.withOut(stream) {
      print_formula(f)
    }
    stream.toString must equalTo(
      """(p ==> q <=> r /\ s \/ (t <=> ~(~u) /\ v)) /\""" +
        """ (p ==> q <=> r /\ s \/ (t <=> ~(~u) /\ v))""")
  }

  "prop.p002" in {
    val fm = parse_prop_formula("""p ==> q <=> r /\ s \/ (t <=> ~ ~u /\ v)""")
    val f = And(Or(fm, fm), fm)
    f must equalTo(
      And(
        Or(
          Iff(Imp(Atom("p"), Atom("q")), Or(And(Atom("r"), Atom("s")), Iff(Atom("t"), And(Not(Not(Atom("u"))), Atom("v"))))),
          Iff(Imp(Atom("p"), Atom("q")), Or(And(Atom("r"), Atom("s")), Iff(Atom("t"), And(Not(Not(Atom("u"))), Atom("v")))))),
        Iff(Imp(Atom("p"), Atom("q")), Or(And(Atom("r"), Atom("s")), Iff(Atom("t"), And(Not(Not(Atom("u"))), Atom("v")))))))
    val stream = new java.io.ByteArrayOutputStream()
    Console.withOut(stream) {
      print_formula(f)
    }
    stream.toString must equalTo(
      """((p ==> q <=> r /\ s \/ (t <=> ~(~u) /\ v)) \/ """ +
        """(p ==> q <=> r /\ s \/ (t <=> ~(~u) /\ v))) /\ """ +
        """(p ==> q <=> r /\ s \/ (t <=> ~(~u) /\ v))""")
  }

  "prop.p003" in {
    false & false must equalTo(false)
  }

  "prop.p004" in {
    false & true must equalTo(false)
  }

  "prop.p005" in {
    true & false must equalTo(false)
  }

  "prop.p006" in {
    true & true must equalTo(true)
  }

  "prop.p007" in {
    eval("""p /\ q ==> q /\ r""")(_ match {
      case "p" ⇒ true
      case "q" ⇒ false
      case "r" ⇒ true
    }) must equalTo(true)
  }

  "prop.p008" in {
    eval("""p /\ q ==> q /\ r"""")(_ match {
      case "p" ⇒ true
      case "q" ⇒ true
      case "r" ⇒ false
    }) must equalTo(false)
  }

  "prop.p009" in {
    atoms("""p /\ q \/ s ==> ~p \/ (r <=> s)""") must equalTo(List("p", "q", "r", "s"))
  }

  "prop.p014" in {
    tautology("""p \/ ~p""") must equalTo(true)
  }
  "prop.p015" in {
    tautology("""p \/ q ==> p""") must equalTo(false)
  }
  "prop.p016" in {
    tautology("""p \/ q ==> q \/ (p <=> q)""") must equalTo(false)
  }
  "prop.p017" in {
    tautology("""(p \/ q) /\ ~(p /\ q) ==> (~p <=> q)""") must equalTo(true)
  }
  "prop.p019" in {
    tautology("""(p ==> q) \/ (q ==> p)""") must equalTo(true)
  }
  "prop.p020" in {
    tautology("""p \/ (q <=> r) <=> (p \/ q <=> p \/ r)""") must equalTo(true)
  }
  "prop.p021" in {
    tautology("""p /\ q <=> ((p <=> q) <=> p \/ q)""") must equalTo(true)
  }
  "prop.p022" in {
    tautology("""(p ==> q) <=> (~q ==> ~p)""") must equalTo(true)
  }
  "prop.p023" in {
    tautology("""(p ==> ~q) <=> (q ==> ~p)""") must equalTo(true)
  }
  "prop.p024" in {
    tautology("""(p ==> q) <=> (q ==> p)""") must equalTo(false)
  }
  "prop.p030" in {
    tautology("""(p ==> p') /\ (q ==> q') ==> (p /\ q ==> p' /\ q')""") must equalTo(true)
  }
  "prop.p031" in {
    tautology("""(p ==> p') /\ (q ==> q') ==> (p \/ q ==> p' \/ q')""") must equalTo(true)
  }

  "prop.026" in {
    dual("""p \/ ~p""") must equalTo(parse_prop_formula("""p /\ ~p"""))
  }

  "prop.027" in {
    psimplify("""(true ==> (x <=> false)) ==> ~(y \/ false /\ z)""") must equalTo(parse_prop_formula("""~x ==> ~y"""))
  }
  "prop.028" in {
    psimplify("""((x ==> y) ==> true) \/ ~false""") must equalTo(parse_prop_formula("true"))
  }

  "prop.029" in {
    tautology(Iff("""(p <=> q) <=> ~(r ==> s)""", nnf("""(p <=> q) <=> ~(r ==> s)"""))) must equalTo(true)
  }

  "prop.035" in {
    tautology(Iff(rawdnf("""(p \/ q /\ r) /\ (~p \/ ~r)"""),
      """(p /\ ~p \/ (q /\ r) /\ ~p) \/ p /\ ~r \/ (q /\ r) /\ ~r""")) must equalTo(true)
  }

  "prop.038" in {
    purednf("""(p \/ q /\ r) /\ (~p \/ ~r)""").filter(!trivial(_)) must equalTo(
      List(List(Atom("p"), Not(Atom("r"))), List(Atom("q"), Atom("r"), Not(Atom("p")))))
  }
}