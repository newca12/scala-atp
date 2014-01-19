/*package org.edla.port.atp

import org.junit.runner.RunWith
import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.runner.JUnitRunner
import org.edla.port.atp.Prop._
import org.edla.study.parsing.parboiled.PropositionalLogic
import org.parboiled2._
import org.edla.study.parsing.parboiled.AST
import scala.util.{ Failure, Success }
import org.edla.study.parsing.parboiled.AST.Expr

@RunWith(classOf[JUnitRunner])
class PropSpec extends SpecificationWithJUnit {

  //sequential
  """Tautology p \/ ~p should be true""" in {
    AST.varNames.clear
    val result = parse("""p \/ ~p""")
    tautology(result, AST.varNames.toArray.sorted) must equalTo(true)
  }

  """Tautology p \/ q ==> p should be false""" in {
    AST.varNames.clear
    val result = parse("""p \/ q ==> p""")
    tautology(result, AST.varNames.toArray.sorted) must equalTo(false)
  }

  """Tautology p \/ q ==> q \/ (p <=> q) should be false""" in {
    AST.varNames.clear
    val result = parse("""p \/ q ==> q \/ (p <=> q)""")
    tautology(result, AST.varNames.toArray.sorted) must equalTo(false)
  }

  "Tautology (p ∨ q) ∧ ¬(p ∧ q) ⇒ (¬p ⇔ q) should be true" in {
    AST.varNames.clear
    val result = parse("""(p \/ q) /\ ~(p /\ q) ==> (~p <=> q)""")
    tautology(result, AST.varNames.toArray.sorted) must equalTo(true)
  }

  def parse(s: String): Expr = {
    val parser = new PropositionalLogic(s)
    parser.expr.run() match {
      case Success(result)        ⇒ result
      case Failure(e: ParseError) ⇒ sys.error(parser.formatError(e, showTraces = true))
      case Failure(e)             ⇒ throw e
    }
  }
}*/ 