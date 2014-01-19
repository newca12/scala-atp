// ========================================================================= //
// Copyright (c) 2003-2007, John Harrison.                                   //
// Copyright (c) 2012-2014, Olivier ROLAND.                                  //
// (See "LICENSE.txt" for details.)                                          //
// ========================================================================= //

package org.edla.port.atp

import org.edla.study.parsing.parboiled.AST.And
import org.edla.study.parsing.parboiled.AST.Atom
import org.edla.study.parsing.parboiled.AST.Formula
import org.edla.study.parsing.parboiled.AST.Iff
import org.edla.study.parsing.parboiled.AST.Imp
import org.edla.study.parsing.parboiled.AST.Not
import org.edla.study.parsing.parboiled.AST.Or

object Formulas {

  implicit def atomOrdering: Ordering[Atom] = new Ordering[Atom] { def compare(a: Atom, b: Atom) = a.name compare b.name }

  // pg. 31
  // ------------------------------------------------------------------------- //
  // Apply a function to the atoms, otherwise keeping structure.               //
  // ------------------------------------------------------------------------- //

  def onatoms(f: String ⇒ Formula, fm: Formula): Formula = {
    fm match {
      case Atom(a)   ⇒ f(a)
      case Not(p)    ⇒ Not(onatoms(f, p))
      case And(p, q) ⇒ And(onatoms(f, p), onatoms(f, q))
      case Or(p, q)  ⇒ Or(onatoms(f, p), onatoms(f, q))
      case Imp(p, q) ⇒ Imp(onatoms(f, p), onatoms(f, q))
      case Iff(p, q) ⇒ Iff(onatoms(f, p), onatoms(f, q))
      case _         ⇒ fm
    }
  }

  // pg. 31
  // ------------------------------------------------------------------------- //
  // Formula analog of list iterator "List.foldBack".                          //
  // ------------------------------------------------------------------------- //

  def overatoms(f: Atom ⇒ List[Atom], fm: Formula, b: List[Atom]): List[Atom] = {
    fm match {
      case Atom(a)   ⇒ f(Atom(a)) ++ b
      case Not(p)    ⇒ overatoms(f, p, b)
      case And(p, q) ⇒ overatoms(f, p, overatoms(f, q, b))
      case Or(p, q)  ⇒ overatoms(f, p, overatoms(f, q, b))
      case Imp(p, q) ⇒ overatoms(f, p, overatoms(f, q, b))
      case Iff(p, q) ⇒ overatoms(f, p, overatoms(f, q, b))
      case _         ⇒ b
    }
  }

  // pg. 32
  // ------------------------------------------------------------------------- //
  // Special case of a union of the results of a function over the atoms.      //
  // ------------------------------------------------------------------------- //  

  def atom_union(f: Atom ⇒ List[Atom], fm: Formula) = overatoms(f, fm, Nil).toSet.toList.sorted

}