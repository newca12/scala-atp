package org.edla.study.parsing.thesis.labun.lambda

import util.parsing.combinator.syntactical.StandardTokenParsers
import util.parsing.combinator.ImplicitConversions

object Parser extends StandardTokenParsers with ImplicitConversions {

  import AST._

  // API
  def parse(input: String): Either[String, Expr] =
    phrase(program)(new lexical.Scanner(input)) match {
      case Success(ast, _) ⇒ Right(ast)
      case e: NoSuccess    ⇒ Left("parser error: " + e.msg)
    }

  // Impl.
  lexical.reserved ++= ("if then else" split ' ')
  lexical.delimiters ++= ("\\ => + - * / ( ) , == = ;" split ' ')

  type P[+T] = Parser[T] // alias for brevity

  def program       = rep1sep(expr, ";") <~ opt(";") ^^ Sequence
  def expr: P[Expr] = lambda | ifExpr | assign | operations

  def lambda     = ("\\" ~> repsep(ident, ",")) ~ ("=>" ~> expr) ^^ Lambda
  def ifExpr     = ("if" ~> expr) ~ ("then" ~> expr) ~ ("else" ~> expr) ^^ IfExpr
  def assign     = ident ~ ("=" ~> expr) ^^ Assign
  def operations = infixOps

  def infixOps = equality
  def equality = sum * ("==" ^^^ Equal)
  def sum      = product * ("+" ^^^ Add | "-" ^^^ Sub)
  def product  = postfixOps * ("*" ^^^ Mul | "/" ^^^ Div)

  def postfixOps = application

  def application = simpleExpr ~ rep(argList) ^^ { case e ~ args ⇒ (e /: args)(Application) }

  def argList = "(" ~> repsep(expr, ",") <~ ")" | simpleExpr ^^ { List(_) }

  def simpleExpr =
    (ident ^^ Var
      | numericLit ^^ { x ⇒
        Lit(x.toInt)
      }
      | stringLit ^^ Lit
      | "(" ~> expr <~ ")"
      | failure("Expression expected"))
}
