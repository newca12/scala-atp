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

  sealed abstract class Formula {
    override def toString = {
      val stream = new java.io.ByteArrayOutputStream()
      Console.withOut(stream) {
        print_formula(this)
      }
      stream.toString
    }
  }

  case object False extends Formula
  case object True extends Formula
  case class Atom(name: String) extends Formula
  case class Not(v: Formula) extends Formula
  case class And(l: Formula, r: Formula) extends Formula
  case class Or(l: Formula, r: Formula) extends Formula
  case class Imp(l: Formula, r: Formula) extends Formula
  case class Iff(l: Formula, r: Formula) extends Formula

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

  // pg. 626
  // ------------------------------------------------------------------------- //
  // Printing of formulas, parametrized by atom printer.                       //
  // ------------------------------------------------------------------------- //

  // NOTE: No use of OCaml format module. i.e. print_box removed during conversion
  def bracket[T](p: Boolean, n: Int, f: (T, Formula) ⇒ Unit, x: T, y: Formula) = {
    if (p) print("(")
    f(x, y)
    if (p) print(")")
  }

  def print_formula(pfn: Formula) = {
    def print_formula_(pr: Int, fm: Formula): Unit = {
      def print_prefix(newpr: Int)(sym: String, p: Formula) = {
        print(sym); print_formula_(newpr + 1, p)
      }
      def print_infix(newpr: Int, sym: String)(p: Formula, q: Formula) = {
        print_formula_(newpr + 1, p)
        print(s" ${sym} ")
        print_formula_(newpr, q)
      }

      fm match {
        case False       ⇒ print("false")
        case True        ⇒ print("true")
        case Atom(pargs) ⇒ print(pargs)
        case Not(p)      ⇒ bracket(pr > 10, 1, print_prefix(10), "~", p)
        case And(p, q)   ⇒ bracket(pr > 8, 0, print_infix(8, """/\"""), p, q)
        case Or(p, q)    ⇒ bracket(pr > 6, 0, print_infix(6, """\/"""), p, q)
        case Imp(p, q)   ⇒ bracket(pr > 4, 0, print_infix(4, "==>"), p, q)
        case Iff(p, q)   ⇒ bracket(pr > 2, 0, print_infix(2, "<=>"), p, q)
      }
    }
    print_formula_(0, pfn)
  }
}