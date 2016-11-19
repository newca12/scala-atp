package org.edla.study.parsing.thesis.labun

import util.parsing.combinator.RegexParsers
import collection._

object PropositionalLogic extends RegexParsers {
  // Parser
  def expr  = xor
  def xor   = rep1sep(equiv, "^") ^^ { _.reduceLeft(Xor) }
  def equiv = rep1sep(impl, "<->") ^^ { _.reduceLeft(Equiv) }
  def impl  = rep1sep(or, "->") ^^ { _.reduceRight(Impl) } // right-associative
  def or    = rep1sep(and, "|") ^^ { _.reduceLeft(Or) } // via right folding
  def and   = rep1sep(not, "&") ^^ { _.reduceLeft(And) }
  def not = opt("!") ~ atom ^^ {
    case Some(_) ~ x ⇒ Not(x)
    case _ ~ x       ⇒ x
  }
  //recursive method expr needs result type
  def atom: Parser[Expr] =
    (const ^^ Const
      | id ^^ Id
      | "(" ~> expr <~ ")"
      | "[" ~> expr <~ "]")
  def const = "T" | "F"
  def id    = """[a-z]\w*""".r

  // AST
  sealed abstract class Expr
  case class Xor(l: Expr, r: Expr)   extends Expr
  case class Equiv(l: Expr, r: Expr) extends Expr
  case class Impl(l: Expr, r: Expr)  extends Expr
  case class Or(l: Expr, r: Expr)    extends Expr
  case class And(l: Expr, r: Expr)   extends Expr
  case class Not(v: Expr)            extends Expr
  case class Const(name: String) extends Expr {
    override def toString = name
  }
  case class Id(name: String) extends Expr {
    varNames += name; override def toString = name
  }

  var varNames = mutable.Set[String]()
  // Interpreter
  def eval(e: Expr): Boolean = e match {
    case Xor(l, r)   ⇒ eval(l) ^ eval(r)
    case Equiv(l, r) ⇒ eval(l) == eval(r)
    case Impl(l, r)  ⇒ !eval(l) || eval(r)
    case Or(l, r)    ⇒ eval(l) || eval(r)
    case And(l, r)   ⇒ eval(l) && eval(r)
    case Not(x)      ⇒ !eval(x)
    case Const(x)    ⇒ x == "T"
    case Id(x)       ⇒ varValues(x)
  }

  var varValues = mutable.Map[String, Boolean]()

  // process
  def process(input: String) = {
    println("input: " + input)
    varNames.clear
    varValues.clear
    val res = parseAll(expr, input)
    println("result: " + res)
    if (res.successful)
      printTruthTable(res.get, varNames.toArray.sorted)
    println()
  }
  // printTruthTable
  def printTruthTable(tree: Expr, varNames: Array[String]) = {
    val (varCount, resLabel, colSpace, resColSpace) =
      (varNames.length, "Result", " ", " ")

    // Header
    println("TRUTH TABLE:\n------------")
    println(varNames.mkString(colSpace) + resColSpace + resLabel)

    // Body
    val rowCount = 1 << varCount
    for (r ← 0 until rowCount) {
      // Evaluate the expression -tree for each state of variables and print the result:
      // State of variables as an array of Booleans
      val state = Array.tabulate[Boolean](varCount)(i ⇒ (r & (1 << i)) > 0)
      // Store the current state of variables in the varVals map
      varValues ++= varNames.zip(state)
      print(varNames map (v ⇒ centered(varValues(v), v.length)) mkString colSpace)
      println(resColSpace + centered(eval(tree), resLabel.length))
    }

    // helper functions
    /**
      * Returns the Boolean value <code>v</code> printed as "0" or "1"
      *
      *
      *
      * in the middle of a String of length <code>w</code >.
      */
    def centered(v: Boolean, w: Int) = {
      val spaceBefore = (w - 1) / 2; val spaceAfter = w - spaceBefore - 1
      /*val buf = new StringBuilder;
      // '1' in the prev. line is the length of a boolean if printed as "0" or "1"
      for (i <- 0 until spaceBefore) buf += ' '
      // We don't use (" " * Int) since it clashes with * method in Parsers
      buf += (if (v) '1' else '0')
      for (i <- 0 until spaceAfter) buf += ' '
      buf.toString*/
      (" " * spaceBefore) + (if (v) '1' else '0') + (" " * spaceAfter)
    }
  }

  def main(args: Array[String]) = {
    if (args.length > 0)
      process(args(0))
  }
}
