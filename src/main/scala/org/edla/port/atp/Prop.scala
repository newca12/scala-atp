// ========================================================================= //
// Copyright (c) 2003-2007, John Harrison.                                   //
// Copyright (c) 2012-2014, Olivier ROLAND.                                  //
// (See "LICENSE.txt" for details.)                                          //
// ========================================================================= //

package org.edla.port.atp

import scala.util.Failure
import scala.util.Success

import org.edla.port.atp.Formulas.And
import org.edla.port.atp.Formulas.Atom
import org.edla.port.atp.Formulas.False
import org.edla.port.atp.Formulas.Formula
import org.edla.port.atp.Formulas.Iff
import org.edla.port.atp.Formulas.Imp
import org.edla.port.atp.Formulas.Not
import org.edla.port.atp.Formulas.Or
import org.edla.port.atp.Formulas.True
import org.edla.port.atp.Formulas.atom_union
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

  // pg. 50
  // ------------------------------------------------------------------------- //
  // Routine simplification.                                                   //
  // ------------------------------------------------------------------------- //

  //https://issues.scala-lang.org/browse/SUGGEST-25
  def psimplify1(fm: Formula): Formula = {
    fm match {
      case Not(False())                      ⇒ True()
      case Not(True())                       ⇒ False()
      case Not(Not(p))                       ⇒ p
      case And(_, False()) | And(False(), _) ⇒ False()
      case And(p, True())                    ⇒ p
      case And(True(), p)                    ⇒ p
      case Or(p, False())                    ⇒ p
      case Or(False(), p)                    ⇒ p
      case Or(_, True()) | Or(True(), _)     ⇒ True()
      case Imp(False(), _) | Imp(_, True())  ⇒ True()
      case Imp(True(), p)                    ⇒ p
      case Imp(p, False())                   ⇒ Not(p)
      case Iff(p, True())                    ⇒ p
      case Iff(True(), p)                    ⇒ p
      case Iff(p, False())                   ⇒ Not(p)
      case Iff(False(), p)                   ⇒ Not(p)
      case _                                 ⇒ fm
    }
  }

  def psimplify(fm: Formula): Formula = {
    fm match {
      case Not(p)    ⇒ psimplify1(Not(psimplify(p)))
      case And(p, q) ⇒ psimplify1(And(psimplify(p), psimplify(q)))
      case Or(p, q)  ⇒ psimplify1(Or(psimplify(p), psimplify(q)))
      case Imp(p, q) ⇒ psimplify1(Imp(psimplify(p), psimplify(q)))
      case Iff(p, q) ⇒ psimplify1(Iff(psimplify(p), psimplify(q)))
      case _         ⇒ fm
    }
  }

  // pg. 52
  // ------------------------------------------------------------------------- //
  // Negation normal form.                                                     //
  // ------------------------------------------------------------------------- //  

  // NOTE: Changed name from nnf to nenfOrig to avoid Scala compiler error.
  def nnfOrig(fm: Formula): Formula = {
    fm match {
      case And(p, q)      ⇒ And(nnfOrig(p), nnfOrig(q))
      case Or(p, q)       ⇒ Or(nnfOrig(p), nnfOrig(q))
      case Imp(p, q)      ⇒ Or(nnfOrig(Not(p)), nnfOrig(q))
      case Iff(p, q)      ⇒ Or(And(nnfOrig(p), nnfOrig(q)), And(nnfOrig(Not(p)), nnfOrig(Not(q))))
      case Not(Not(p))    ⇒ nnfOrig(p)
      case Not(And(p, q)) ⇒ Or(nnfOrig(Not(p)), nnfOrig(Not(q)))
      case Not(Or(p, q))  ⇒ And(nnfOrig(Not(p)), nnfOrig(Not(q)))
      case Not(Imp(p, q)) ⇒ And(nnfOrig(p), nnfOrig(Not(q)))
      case Not(Iff(p, q)) ⇒ Or(And(nnfOrig(p), nnfOrig(Not(q))), And(nnfOrig(Not(p)), nnfOrig(q)))
      case _              ⇒ fm
    }
  }

  // pg. 52
  // ------------------------------------------------------------------------- //
  // Roll in simplification.                                                   //
  // ------------------------------------------------------------------------- //  

  def nnf(fm: Formula): Formula = nnfOrig(psimplify(fm))

  // pg. 53
  // ------------------------------------------------------------------------- //
  // Simple negation-pushing when we don't care to distinguish occurrences.    //
  // ------------------------------------------------------------------------- //

  // NOTE: Changed name from nenf to nenfOrig to avoid Scala compiler error.
  def nenfOrig(fm: Formula): Formula = {
    fm match {
      case Not(Not(p))    ⇒ nenfOrig(p)
      case Not(And(p, q)) ⇒ Or(nenfOrig(Not(p)), nenfOrig(Not(q)))
      case Not(Or(p, q))  ⇒ And(nenfOrig(Not(p)), nenfOrig(Not(q)))
      case Not(Imp(p, q)) ⇒ And(nenfOrig(p), nenfOrig(Not(q)))
      case Not(Iff(p, q)) ⇒ Iff(nenfOrig(p), nenfOrig(Not(q)))
      case And(p, q)      ⇒ And(nenfOrig(p), nenfOrig(q))
      case Or(p, q)       ⇒ Or(nenfOrig(p), nenfOrig(q))
      case Imp(p, q)      ⇒ Or(nenfOrig(Not(p)), nenfOrig(q))
      case Iff(p, q)      ⇒ Iff(nenfOrig(p), nenfOrig(q))
      case _              ⇒ fm
    }
  }

  def nenf(fm: Formula): Formula = nenfOrig(psimplify(fm))

}