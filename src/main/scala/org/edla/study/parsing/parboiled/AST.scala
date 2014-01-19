package org.edla.study.parsing.parboiled

import scala.collection.mutable

object AST {

  sealed abstract class Prop
  case class P(pname: String) extends Prop

  sealed abstract class Formula
  case class False extends Formula
  case class True extends Formula
  case class Atom(name: String) extends Formula {
    override def toString = name
  }
  case class Not(v: Formula) extends Formula
  case class And(l: Formula, r: Formula) extends Formula
  case class Or(l: Formula, r: Formula) extends Formula
  case class Imp(l: Formula, r: Formula) extends Formula
  case class Iff(l: Formula, r: Formula) extends Formula

}