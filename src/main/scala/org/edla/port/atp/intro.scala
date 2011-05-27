package org.edla.port.atp

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.combinator.lexical.StdLexical

object Intro extends StandardTokenParsers {

  abstract class Expression
  case class Var(n: String) extends Expression
  case class Const(v: Int) extends Expression
  case class Add(l: Expression, r: Expression) extends Expression
  case class Mul(l: Expression, r: Expression) extends Expression

  def simplify1(e: Expression): Expression = e match {
    case Add(Const(m), Const(n)) => Const(m + n)
    case Mul(Const(m), Const(n)) => Const(m * n)
    case Add(Const(0), x) => x
    case Add(x, Const(0)) => x
    case Mul(Const(0), x) => Const(0)
    case Mul(x, Const(0)) => Const(0)
    case Mul(Const(1), x) => x
    case Mul(x, Const(1)) => x
    case x: Any => x
  }

  def simplify(e: Expression): Expression = e match {
    case Add(e1, e2) => simplify1(Add(simplify(e1), simplify(e2)))
    case Mul(e1, e2) => simplify1(Mul(simplify(e1), simplify(e2)))
    case x: Any => x
  }

  def lex(input: String): List[String] = {
    var l = List("")
    lexical.delimiters ++= List("(", ")", "+", "-", "*", "/", "'", "++", "==", "--")
    var s: lexical.Scanner = new lexical.Scanner(input)
    l = l.init
    l = l ::: List(s.first.chars.toString)
    do {
      s = s.rest
      l = l ::: List(s.first.chars.toString)
    } while (!s.atEnd)
    return l.init
  }
}