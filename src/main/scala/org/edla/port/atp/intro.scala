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
    lexical.delimiters ++= List("(", ")", "+", "-", "*", "/", "'", "++", "==", "--")
    var s = new lexical.Scanner(input)
    var l = List(s.first.chars.toString)
    do {
      s = s.rest
      l = l ::: List(s.first.chars.toString)
    } while (!s.atEnd)
    return l.init
  }

  def constant = numericLit ^^ { s => Const(s.toInt) }

  def variable = ident ^^ { s => Var(s.toString) }

  def expression: Parser[Expression] = (product ~ "+" ~ product) ^^ {
    case left ~ "+" ~ right =>
      Add(left, right)
  } | product
  
  def product: Parser[Expression] = (atom ~ "*" ~ atom) ^^ {
    case left ~ "*" ~ right =>
      Mul(left, right)
  } | atom
  
  def atom: Parser[Expression] = "(" ~> expression <~ ")" | constant | variable

  def parse(s: String) = {
    lexical.delimiters ++= List("+", "*", "(", ")")
    val tokens = new lexical.Scanner(s)
    phrase(expression)(tokens)
  }

  def apply(s: String): Expression = {
    parse(s) match {
      case Success(tree, _) => tree
      case e: NoSuccess =>
        throw new IllegalArgumentException("Bad syntax: " + s)
    }
  }
  
  def parseExpression(exprstr: String): Expression = {
    (parse(exprstr): @unchecked) match {
      case Success(tree, _) =>
        return tree
    }
  }

  def stringOfExp(pr: Int, e: Expression): String = {
    e match {
      case Var(n: String) => n
      case Const(v: Int) => v.toString
      case Add(l: Expression, r: Expression) => {
        val s: String = stringOfExp(3, l) + " + " + stringOfExp(2, r);
        if (2 < pr) "(" + s + ")" else s;

      }
      case Mul(l: Expression, r: Expression) => {
        val s: String = stringOfExp(5, l) + " * " + stringOfExp(4, r);
        if (4 < pr) "(" + s + ")" else s;
      }
    }
  }
}