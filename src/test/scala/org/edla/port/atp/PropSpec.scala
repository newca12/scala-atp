package org.edla.port.atp

import org.edla.port.atp.Formulas.{ And, Atom, Iff, Imp, Not, Or, print_formula }
import org.edla.port.atp.Prop.{ atoms, dual, eval, nnf, parse_prop_formula, psimplify, purednf, rawdnf, tautology, trivial }
import org.scalatest.{ Finders, FunSuite }

class PropSpec extends FunSuite {

  implicit def default_parser(s: String) = parse_prop_formula(s)

  test("prop.p001") {
    val fm = parse_prop_formula("""p ==> q <=> r /\ s \/ (t <=> ~ ~u /\ v)""")
    val f = And(fm, fm)
    f ===
      And(
        Iff(Imp(Atom("p"), Atom("q")), Or(And(Atom("r"), Atom("s")), Iff(Atom("t"), And(Not(Not(Atom("u"))), Atom("v"))))),
        Iff(Imp(Atom("p"), Atom("q")), Or(And(Atom("r"), Atom("s")), Iff(Atom("t"), And(Not(Not(Atom("u"))), Atom("v")))))
      )

    val stream = new java.io.ByteArrayOutputStream()
    Console.withOut(stream) {
      print_formula(f)
    }
    stream.toString ===
      """(p ==> q <=> r /\ s \/ (t <=> ~(~u) /\ v)) /\""" +
      """ (p ==> q <=> r /\ s \/ (t <=> ~(~u) /\ v))"""
  }

  test("prop.p002") {
    val fm = parse_prop_formula("""p ==> q <=> r /\ s \/ (t <=> ~ ~u /\ v)""")
    val f = And(Or(fm, fm), fm)
    f ===
      And(
        Or(
          Iff(Imp(Atom("p"), Atom("q")), Or(And(Atom("r"), Atom("s")), Iff(Atom("t"), And(Not(Not(Atom("u"))), Atom("v"))))),
          Iff(Imp(Atom("p"), Atom("q")), Or(And(Atom("r"), Atom("s")), Iff(Atom("t"), And(Not(Not(Atom("u"))), Atom("v")))))
        ),
        Iff(Imp(Atom("p"), Atom("q")), Or(And(Atom("r"), Atom("s")), Iff(Atom("t"), And(Not(Not(Atom("u"))), Atom("v")))))
      )

    val stream = new java.io.ByteArrayOutputStream()
    Console.withOut(stream) {
      print_formula(f)
    }
    stream.toString ===
      """((p ==> q <=> r /\ s \/ (t <=> ~(~u) /\ v)) \/ """ +
      """(p ==> q <=> r /\ s \/ (t <=> ~(~u) /\ v))) /\ """ +
      """(p ==> q <=> r /\ s \/ (t <=> ~(~u) /\ v))"""
  }

  test("prop.p003") {
    false & false === false
  }

  test("prop.p004") {
    false & true === false
  }

  test("prop.p005") {
    true & false === false
  }

  test("prop.p006") {
    true & true === true
  }

  test("prop.p007") {
    eval("""p /\ q ==> q /\ r""")(_ match {
      case "p" ⇒ true
      case "q" ⇒ false
      case "r" ⇒ true
    }) === true
  }

  test("prop.p008") {
    eval("""p /\ q ==> q /\ r"""")(_ match {
      case "p" ⇒ true
      case "q" ⇒ true
      case "r" ⇒ false
    }) === false
  }

  test("prop.p009") {
    atoms("""p /\ q \/ s ==> ~p \/ (r <=> s)""") === List("p", "q", "r", "s")
  }

  test("prop.p014") {
    tautology("""p \/ ~p""") === true
  }
  test("prop.p015") {
    tautology("""p \/ q ==> p""") === false
  }
  test("prop.p016") {
    tautology("""p \/ q ==> q \/ (p <=> q)""") === false
  }
  test("prop.p017") {
    tautology("""(p \/ q) /\ ~(p /\ q) ==> (~p <=> q)""") === true
  }
  test("prop.p019") {
    tautology("""(p ==> q) \/ (q ==> p)""") === true
  }
  test("prop.p020") {
    tautology("""p \/ (q <=> r) <=> (p \/ q <=> p \/ r)""") === true
  }
  test("prop.p021") {
    tautology("""p /\ q <=> ((p <=> q) <=> p \/ q)""") === true
  }
  test("prop.p022") {
    tautology("""(p ==> q) <=> (~q ==> ~p)""") === true
  }
  test("prop.p023") {
    tautology("""(p ==> ~q) <=> (q ==> ~p)""") === true
  }
  test("prop.p024") {
    tautology("""(p ==> q) <=> (q ==> p)""") === false
  }
  test("prop.p030") {
    tautology("""(p ==> p') /\ (q ==> q') ==> (p /\ q ==> p' /\ q')""") === true
  }
  test("prop.p031") {
    tautology("""(p ==> p') /\ (q ==> q') ==> (p \/ q ==> p' \/ q')""") === true
  }

  test("prop.026") {
    dual("""p \/ ~p""") === parse_prop_formula("""p /\ ~p""")
  }

  test("prop.027") {
    psimplify("""(true ==> (x <=> false)) ==> ~(y \/ false /\ z)""") === parse_prop_formula("""~x ==> ~y""")
  }
  test("prop.028") {
    psimplify("""((x ==> y) ==> true) \/ ~false""") === parse_prop_formula("true")
  }

  test("prop.029") {
    tautology(Iff("""(p <=> q) <=> ~(r ==> s)""", nnf("""(p <=> q) <=> ~(r ==> s)"""))) === true
  }

  test("prop.035") {
    tautology(Iff(
      rawdnf("""(p \/ q /\ r) /\ (~p \/ ~r)"""),
      """(p /\ ~p \/ (q /\ r) /\ ~p) \/ p /\ ~r \/ (q /\ r) /\ ~r"""
    )) === true
  }

  test("prop.038") {
    purednf("""(p \/ q /\ r) /\ (~p \/ ~r)""").filter(!trivial(_)) ===
      List(List(Atom("p"), Not(Atom("r"))), List(Atom("q"), Atom("r"), Not(Atom("p"))))
  }
}