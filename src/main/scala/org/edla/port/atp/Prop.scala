// ========================================================================= //
// Copyright (c) 2003-2007, John Harrison.                                   //
// Copyright (c) 2012-2014, Olivier ROLAND.                                  //
// (See "LICENSE.txt" for details.)                                          //
// ========================================================================= //

package org.edla.port.atp

import scala.util.Failure
import scala.util.Success

import org.edla.port.atp.Formulas.atom_union
import org.edla.study.parsing.parboiled.AST.And
import org.edla.study.parsing.parboiled.AST.Atom
import org.edla.study.parsing.parboiled.AST.False
import org.edla.study.parsing.parboiled.AST.Formula
import org.edla.study.parsing.parboiled.AST.Iff
import org.edla.study.parsing.parboiled.AST.Imp
import org.edla.study.parsing.parboiled.AST.Not
import org.edla.study.parsing.parboiled.AST.Or
import org.edla.study.parsing.parboiled.AST.True
import org.edla.study.parsing.parboiled.PropositionalLogic
import org.parboiled2.ParseError
import org.parboiled2.ParserInput.apply

object Prop {

  // pg. 29
  // ------------------------------------------------------------------------- //
  // Parsing of propositional formulas.                                        //
  // ------------------------------------------------------------------------- //

  def parse_prop_formula(s: String) = {
    val parser = new PropositionalLogic(s)
    val expr = parser.expr.run() match {
      case Success(expr)          ⇒ expr
      case Failure(e: ParseError) ⇒ sys.error(parser.formatError(e, showTraces = true))
      case Failure(e)             ⇒ throw e
    }
    expr
  }

  // pg. 32
  // ------------------------------------------------------------------------- //
  // Interpretation of formulas.                                               //
  // ------------------------------------------------------------------------- //

  def eval(fm: Formula)(v: String ⇒ Boolean): Boolean = {
    fm match {
      case False()   ⇒ false
      case True()    ⇒ true
      case Atom(x)   ⇒ v(x)
      case Not(p)    ⇒ !eval(p)(v)
      case And(p, q) ⇒ eval(p)(v) && eval(q)(v)
      case Or(p, q)  ⇒ eval(p)(v) || eval(q)(v)
      case Imp(p, q) ⇒ !eval(p)(v) || eval(q)(v)
      case Iff(p, q) ⇒ eval(p)(v) == eval(q)(v)
    }
  }

  // pg. 35
  // ------------------------------------------------------------------------- //
  // Return the set of propositional variables in a formula.                   //
  // ------------------------------------------------------------------------- //

  def atoms(fm: Formula) = atom_union((a: Atom) ⇒ (a :: Nil), fm) map (_.name)

  // pg. 35
  // ------------------------------------------------------------------------- //
  // Code to print out truth tables.                                           //
  // ------------------------------------------------------------------------- // 

  def onallvaluations(subfn: (String ⇒ Boolean) ⇒ Boolean, v: String ⇒ Boolean, ats: List[String]): Boolean = {
    ats match {
      case Nil ⇒ subfn(v)
      case p :: ps ⇒ {
        def v_(t: Boolean)(q: String) = if (q == p) t else v(q)
        onallvaluations(subfn, v_(false), ps) && onallvaluations(subfn, v_(true), ps)
      }
    }
  }

  def print_truthtable(fm: Formula): Unit = {
    val ats = atoms(fm)
    val width = ats.foldRight(0)((x, y) ⇒ Math.max(x.length, y)) + 5 + 1
    def fixw(s: String) = s"""${s}${" " * (width - s.length)}"""
    def truthstring(p: Boolean) = fixw(if (p) "true" else "false")
    def lis(v: String ⇒ Boolean) = ats.map(x ⇒ truthstring(v(x)))
    def ans(v: String ⇒ Boolean) = truthstring(eval(fm)(v))
    def mk_row(v: String ⇒ Boolean) = { println(lis(v).foldRight("| " + ans(v))((x, y) ⇒ x + y)); true }
    val separator = "-" * (width * ats.length + 9)
    println(ats.foldRight("| formula")((x, y) ⇒ fixw(x) + y))
    println(separator)
    onallvaluations(mk_row, (s: String) ⇒ false, ats)
    println(separator)
  }

  // pg. 41
  // ------------------------------------------------------------------------- //
  // Recognizing tautologies.                                                  //
  // ------------------------------------------------------------------------- //

  def tautology(fm: Formula) = onallvaluations(eval(fm)_, (s: String) ⇒ false, atoms(fm))

}