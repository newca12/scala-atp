package org.edla.port.atp

import org.junit.runner.RunWith
import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.runner.JUnitRunner
import org.edla.port.atp.Prop._
import org.edla.study.parsing.parboiled.PropositionalLogic
import org.parboiled.scala._
import org.edla.study.parsing.parboiled.AST

@RunWith(classOf[JUnitRunner])
class PropSpec extends SpecificationWithJUnit {

  //sequential
  "Tautology (p | q) & !(p & q) -> (!p <-> q) should be true" in {
    val parser = new PropositionalLogic
    val result = ReportingParseRunner(parser.expr).run("(p | q) & !(p & q) -> (!p <-> q)")
    tautology(result.result.get, AST.varNames.toArray.sorted) must equalTo(true)
  }

  "Tautology (p | q) => q | (p <-> q) should be false" in {
    AST.varNames.clear
    varValues.clear
    val parser = new PropositionalLogic
    val result = ReportingParseRunner(parser.expr).run("(p | q) => q | (p <-> q)")
    tautology(result.result.get, AST.varNames.toArray.sorted) must equalTo(false)
  }

}