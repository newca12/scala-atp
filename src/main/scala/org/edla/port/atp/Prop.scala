package org.edla.port.atp

import scala.Array.canBuildFrom
import scala.Array.fallbackCanBuildFrom
import scala.collection.mutable
import org.parboiled.scala._
import org.edla.study.parsing.parboiled.AST.And
import org.edla.study.parsing.parboiled.AST.Const
import org.edla.study.parsing.parboiled.AST.Equiv
import org.edla.study.parsing.parboiled.AST.Expr
import org.edla.study.parsing.parboiled.AST.Id
import org.edla.study.parsing.parboiled.AST.Impl
import org.edla.study.parsing.parboiled.AST.Not
import org.edla.study.parsing.parboiled.AST.Or
import org.edla.study.parsing.parboiled.AST.Xor
import org.edla.study.parsing.parboiled.PropositionalLogic
import org.edla.study.parsing.parboiled.AST

object Prop {

  val parser = new PropositionalLogic

  def eval(e: Expr): Boolean = e match {
    case Xor(l, r) => eval(l) ^ eval(r)
    case Equiv(l, r) => eval(l) == eval(r)
    case Impl(l, r) => !eval(l) || eval(r)
    case Or(l, r) => eval(l) || eval(r)
    case And(l, r) => eval(l) && eval(r)
    case Not(x) => !eval(x)
    case Const(x) => x == "T"
    case Id(x) => varValues(x)
  }

  var varValues = mutable.Map[String, Boolean]()

  def printTruthTable(tree: Expr, varNames: Array[String]) {
    val (varCount, resLabel, colSpace, resColSpace) =
      (varNames.length, "Result", " ", " ")

    // Header
    println("TRUTH TABLE:\n------------")
    println(varNames.mkString(colSpace) + resColSpace + resLabel)

    // Body
    val rowCount = 1 << varCount
    for (r <- 0 until rowCount) {
      // Evaluate the expression -tree for each state of variables and print the result:
      // State of variables as an array of Booleans
      val state = Array.tabulate[Boolean](varCount)(i => (r & (1 << i)) > 0)
      // Store the current state of variables in the varVals map
      varValues ++= varNames.zip(state)
      print(varNames map (v => centered(varValues(v), v.length)) mkString colSpace)
      println(resColSpace + centered(eval(tree), resLabel.length))
    }

    // helper functions
    /**
     * Returns the Boolean value <code>v</code> printed as "0" or "1"
     *
     *
     *
     * in the middle of a String of length <code>w</code >.
     */
    def centered(v: Boolean, w: Int) = {
      val spaceBefore = (w - 1) / 2; val spaceAfter = w - spaceBefore - 1
      /*val buf = new StringBuilder;
      // '1' in the prev. line is the length of a boolean if printed as "0" or "1"
      for (i <- 0 until spaceBefore) buf += ' '
      // We don't use (" " * Int) since it clashes with * method in Parsers
      buf += (if (v) '1' else '0')
      for (i <- 0 until spaceAfter) buf += ' '
      buf.toString*/
      (" " * spaceBefore) + (if (v) '1' else '0') + (" " * spaceAfter)
    }
  }

  def tautology(tree: Expr, varNames: Array[String]): Boolean = {
    varValues.clear
    val varCount = varNames.length
    val rowCount = 1 << varCount
    for (r <- 0 until rowCount) {
      val state = Array.tabulate[Boolean](varCount)(i => (r & (1 << i)) > 0)
      varValues ++= varNames.zip(state)
      if (eval(tree) == false) return false
    }
    return true
  }

  def parse_prop_formula(s: String) = BasicParseRunner(parser.expr).run(s).result.get

  def main(args: Array[String]) {
    if (args.length == 0) sys.exit
    val parser = new PropositionalLogic
    val result = ReportingParseRunner(parser.expr).run(args(0))
    printTruthTable(result.result.get, AST.varNames.toArray.sorted)
    println(tautology(result.result.get, AST.varNames.toArray.sorted))
  }

}