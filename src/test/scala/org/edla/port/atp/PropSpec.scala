package org.edla.port.atp

import org.junit.runner.RunWith
import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.runner.JUnitRunner
import org.edla.port.atp.Prop._
import org.edla.study.parsing.parboiled.PropositionalLogic
import org.parboiled2._
import org.edla.study.parsing.parboiled.AST
import scala.util.{ Failure, Success }

@RunWith(classOf[JUnitRunner])
class PropSpec extends SpecificationWithJUnit {

  //sequential
  "Tautology (p ∨ q) ∧ ¬(p ∧ q) ⇒ (¬p ⇔ q) should be true" in {
    AST.varNames.clear
    varValues.clear
    val parser = new PropositionalLogic("""(p \/ q) /\ ~(p /\ q) ==> (~p <=> q)""")
    val result = parser.expr.run() match {
      case Success(result) ⇒
        tautology(result, AST.varNames.toArray.sorted) must equalTo(true)
    }
    result
  }

  "Tautology (p ∨ q) ⇒ q ∨ (p ⇔ q) should be false" in {
    AST.varNames.clear
    varValues.clear
    val parser = new PropositionalLogic("""(p \/ q) ==> q \/ (p <=> q)""")
    val result = parser.expr.run() match {
      case Success(result) ⇒ tautology(result, AST.varNames.toArray.sorted) must equalTo(false)
    }
    result
  }

}