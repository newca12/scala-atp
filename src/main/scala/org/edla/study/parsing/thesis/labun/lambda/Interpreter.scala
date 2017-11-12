package org.edla.study.parsing.thesis.labun.lambda

object Interpreter {
  import AST._
  // API
  def eval(ast: Expr): Unit = {

    try {
      (new Context) eval ast
      ()
    } catch {
      case e: Throwable ⇒
        val msg = e.getMessage
        println("eval error: " + (if (msg != null && msg != "") msg else e))
    }
  }

  // Impl.

  // Type aliases
  type VarName     = String
  type VarValue    = Any
  type Environment = collection.immutable.Map[VarName, VarValue]

  // Runtime types
  sealed abstract class Func extends (List[Any] ⇒ Any)

  case object Println extends Func {
    override def toString      = "println (built-in)"
    def apply(args: List[Any]) = println(args mkString (", "))
  }

  case class Closure(env: Environment, lam: Lambda) extends Func {

    override def toString =
      "Closure(Env(" + env.mkString(",") + "), Params(" +
        lam.params.mkString(",") + "), Body(" + lam.body + "))"

    def apply(args: List[Any]) = {
      if (args.length != lam.params.length)
        sys.error(
          "Wrong number of arguments for function: expected " +
            lam.params.length + ", found " + args.length)
      else (new Context(env ++ lam.params.zip(args))) eval lam.body
    }
  }

  // Evaluation
  val initEnv: Environment = Map("println" → Println)

  class Context(private var env: Environment = initEnv) {
    override def toString = "Environment: " + env.mkString(", ")
    def eval(e: Expr): Any = e match {
      case Sequence(exprs) ⇒ exprs foreach eval
      case lam: Lambda     ⇒ Closure(env, lam)
      case IfExpr(e1, e2, e3) ⇒
        eval(e1) match {
          case b: Boolean ⇒ eval(if (b) e2 else e3)
          case _          ⇒ sys.error("Not a boolean value in condition of IF expression")
        }
      case Assign(id, expr) ⇒ env += (id → eval(expr))

      case Equal(e1, e2) ⇒ eval(e1) == eval(e2)
      case Add(e1, e2) ⇒
        (eval(e1), eval(e2)) match {
          case (v1: String, v2) ⇒ v1 + v2.toString
          case (v1, v2: String) ⇒ v1.toString + v2

          case (i1: Int, i2: Int) ⇒ i1 + i2
          case _                  ⇒ sys.error("'+' requires two Int values or at least one String")
        }
      case Sub(e1, e2) ⇒
        (eval(e1), eval(e2)) match {
          case (i1: Int, i2: Int) ⇒ i1 - i2
          case _                  ⇒ sys.error("'-' requires two Int values")
        }
      case Mul(e1, e2) ⇒
        (eval(e1), eval(e2)) match {
          case (i1: Int, i2: Int) ⇒ i1 * i2
          case _                  ⇒ sys.error("'*' requires two Int values")
        }
      case Div(e1, e2) ⇒
        (eval(e1), eval(e2)) match {
          case (i1: Int, i2: Int) ⇒ i1 / i2
          case _                  ⇒ sys.error("'/' requires two Int values")
        }
      case Application(expr, args) ⇒
        eval(expr) match {
          case f: Func ⇒ f(args map eval)
          case x ⇒
            sys.error(
              expr + " cannot be applied as a function to argument(s) " + args +
                ".\n(Only functions can be applied)")
        }
      case Var(id) ⇒ env getOrElse (id, sys.error("Undefined var " + id))
      case Lit(v)  ⇒ v
    }
  }
}
