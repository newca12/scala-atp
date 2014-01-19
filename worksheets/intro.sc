// ========================================================================= //
// Copyright (c) 2003-2007, John Harrison.                                   //
// Copyright (c) 2012-2014, Olivier ROLAND.                                  //
// (See "LICENSE.txt" for details.)                                          //
// ========================================================================= //

import org.edla.port.atp.Intro._
import org.edla.port.atp.Lex.lex

object intro {
  // pg. 14
  // ------------------------------------------------------------------------- //
  // Trivial example of using the type constructors.                           //
  // ------------------------------------------------------------------------- //

  //intro.p001
  Add(Mul(Const(2), Var("x")), Var("y"))          //> res0: org.edla.port.atp.Intro.Add = Add(Mul(Const(2),Var(x)),Var(y))

  // pg. 16
  // ------------------------------------------------------------------------- //
  // Example.                                                                  //
  // ------------------------------------------------------------------------- //

  val e = Add(Mul(Add(Mul(Const(0), Var("x")), Const(1)), Const(3)), Const(12))
                                                  //> e  : org.edla.port.atp.Intro.Add = Add(Mul(Add(Mul(Const(0),Var(x)),Const(1
                                                  //| )),Const(3)),Const(12))
  //intro.p002
  simplify(e)                                     //> res1: org.edla.port.atp.Intro.Expression = Const(15)

  //intro.p003
  lex("2*((var_1 + x') + 11)").mkString(";")      //> res2: String = 2;*;(;(;var_1;+;x;';);+;11;)

  // intro.p004
  lex("if (*p1-- == *p2++) then f() else g()").mkString(";")
                                                  //> res3: String = if;(;*;p1;--;==;*;p2;++;);then;f;(;);else;g;(;)
  // pg. 20
  // ------------------------------------------------------------------------- //
  // Our parser.                                                               //
  // ------------------------------------------------------------------------- //

  // intro.p005
  parseExpression("x + 1")                        //> res4: org.edla.port.atp.Intro.Expression = Add(Var(x),Const(1))

  // pg. 21
  // ------------------------------------------------------------------------- //
  // Demonstrate automatic installation.                                       //
  // ------------------------------------------------------------------------- //

  // intro.p006
  parseExpression("(x1 + x2 + x3) * (1 + 2 + 3 * x + y)")
                                                  //> res5: org.edla.port.atp.Intro.Expression = Mul(Add(Var(x1),Add(Var(x2),Var(
                                                  //| x3))),Add(Const(1),Add(Const(2),Add(Mul(Const(3),Var(x)),Var(y)))))
  // pg. 21
  // ------------------------------------------------------------------------- //
  // Examples.                                                                 //
  // ------------------------------------------------------------------------- //

  // intro.p008
  printExp(parseExpression("x + 3 * y"))          //> res6: String = x + 3 * y

  // intro.p000
  printExp(parseExpression("(x + 3) * y"))        //> res7: String = (x + 3) * y

  // intro.p010
  printExp(parseExpression("1 + 2 + 3"))          //> res8: String = 1 + 2 + 3

  // intro.p011
  printExp(parseExpression("((1 + 2) + 3) + 4"))  //> res9: String = ((1 + 2) + 3) + 4

  // pg. 22
  // ------------------------------------------------------------------------- //
  // Example shows the problem.                                                //
  // ------------------------------------------------------------------------- //

  // intro.p012
  printExp(parseExpression("(x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10) * (y1 + y2 + y3 + y4 + y5 + y6 + y7 + y8 + y9 + y10)"))
                                                  //> res10: String = (x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10) * (y1 + 
                                                  //| y2 + y3 + y4 + y5 + y6 + y7 + y8 + y9 + y10)
}