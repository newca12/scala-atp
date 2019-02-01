package org.edla.study.parsing.thesis.labun

import util.parsing.combinator.JavaTokenParsers

trait ArithParser extends JavaTokenParsers {
  type T // return type of the expr-parser
  def expr: Parser[T] = chainl1(term, "+" ^^^ Add | "-" ^^^ Sub)
  def term            = chainl1(factor, "*" ^^^ Mul | "/" ^^^ Div)
  def factor          = floatingPointNumber ^^ Number | "(" ~> expr <~ ")"

  // abstract semantic actions
  def Add: (T, T) => T
  def Sub: (T, T) => T
  def Mul: (T, T) => T
  def Div: (T, T) => T
  def Number: String => T

}

trait DirectEvaluation {
  type T = Double
  val Add    = (_: Double) + (_: Double)
  val Sub    = (_: Double) - (_: Double)
  val Mul    = (_: Double) * (_: Double)
  val Div    = (_: Double) / (_: Double)
  val Number = (_: String).toDouble
}

trait ASTBuilding {
  type T = Expr
  sealed abstract class Expr
  case class Add(e1: Expr, e2: Expr) extends Expr
  case class Sub(e1: Expr, e2: Expr) extends Expr
  case class Mul(e1: Expr, e2: Expr) extends Expr
  case class Div(e1: Expr, e2: Expr) extends Expr
  case class Number(e: String)       extends Expr
}

object Interpreter extends ArithParser with DirectEvaluation
object Compiler    extends ArithParser with ASTBuilding
object Arith {
  def main(args: Array[String]) = {
    val arg = args.toList
    val parser: ArithParser = if (arg.head == "eval") {
      println("Now I'm interpreter!"); Interpreter
    } else {
      println("Now I'm compiler!"); Compiler
    }
    arg.tail foreach { x =>
      println("input: " + x)
      println("result: " + parser.parseAll(parser.expr, x))
      println()
    }
  }
}
