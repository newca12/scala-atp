package org.edla.study.parsing.parboiled

import scala.collection.mutable

object AST {

  sealed abstract class Expr
  case class Xor(l: Expr, r: Expr) extends Expr
  case class Equiv(l: Expr, r: Expr) extends Expr
  case class Impl(l: Expr, r: Expr) extends Expr
  case class Or(l: Expr, r: Expr) extends Expr
  case class And(l: Expr, r: Expr) extends Expr
  case class Not(v: Expr) extends Expr
  case class Const(name: String) extends Expr {
    override def toString = name
  }
  case class Id(name: String) extends Expr {
    varNames += name; override def toString = name
  }

  var varNames = mutable.Set[String]()

}