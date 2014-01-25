// ========================================================================= //
// Copyright (c) 2003-2007, John Harrison.                                   //
// Copyright (c) 2012-2014, Olivier ROLAND.                                  //
// (See "LICENSE.txt" for details.)                                          //
// ========================================================================= //

package org.edla.port.atp

object Formulas {

  implicit def atomOrdering: Ordering[Atom] = new Ordering[Atom] { def compare(a: Atom, b: Atom) = a.name compare b.name }

  sealed abstract class Prop
  case class P(pname: String) extends Prop

  sealed abstract class Formula
  case class False extends Formula {
    override def toString = "false"
  }
  case class True extends Formula {
    override def toString = "true"
  }
  case class Atom(name: String) extends Formula {
    override def toString = name
  }
  case class Not(v: Formula) extends Formula {
    override def toString = s"~${v}"
  }
  case class And(l: Formula, r: Formula) extends Formula {
    override def toString = s"""${l} /\\ ${r}"""
  }
  case class Or(l: Formula, r: Formula) extends Formula {
    override def toString = s"""${l} \\/ ${r}"""
  }
  case class Imp(l: Formula, r: Formula) extends Formula {
    override def toString = s"${l} ==> ${r}"
  }
  case class Iff(l: Formula, r: Formula) extends Formula {
    override def toString = s"${l} <=> ${r}"
  }

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