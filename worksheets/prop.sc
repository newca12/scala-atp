// ========================================================================= //
// Copyright (c) 2003-2007, John Harrison.                                   //
// Copyright (c) 2012-2014, Olivier ROLAND.                                  //
// (See "LICENSE.txt" for details.)                                          //
// ========================================================================= //

import org.edla.port.atp.Prop._
import org.edla.port.atp.Formulas._
import com.sun.org.apache.xalan.internal.xsltc.compiler.ForEach

object prop {

  implicit def default_parser(s: String) = parse_prop_formula(s)
                                                  //> default_parser: (s: String)org.edla.port.atp.Formulas.Formula
  // pg. 29
  // ------------------------------------------------------------------------- //
  // Testing the parser and printer                               						 //
  // ------------------------------------------------------------------------- //

  // nota: ~ ~u is not allowed
  val fm001: Formula = """p ==> q <=> r /\ s \/ (t <=> ~(~u) /\ v)"""
                                                  //> fm001  : org.edla.port.atp.Formulas.Formula = p ==> q <=> ( ( r /\ s ) \/ t 
                                                  //| <=> ( ~~u /\ v ) )
  // pg. 30
  // prop.p001
  And(fm001, fm001)                               //> res0: org.edla.port.atp.Formulas.And = ( p ==> q <=> ( ( r /\ s ) \/ t <=> 
                                                  //| ( ~~u /\ v ) ) /\ p ==> q <=> ( ( r /\ s ) \/ t <=> ( ~~u /\ v ) ) )
  // prop.p002
  And(Or(fm001, fm001), fm001)                    //> res1: org.edla.port.atp.Formulas.And = ( ( p ==> q <=> ( ( r /\ s ) \/ t <=
                                                  //| > ( ~~u /\ v ) ) \/ p ==> q <=> ( ( r /\ s ) \/ t <=> ( ~~u /\ v ) ) ) /\ p
                                                  //|  ==> q <=> ( ( r /\ s ) \/ t <=> ( ~~u /\ v ) ) )
  // pg. 33
  // prop.p003
  false && false                                  //> res2: Boolean(false) = false

  // prop.p004
  false && true                                   //> res3: Boolean(false) = false

  // prop.p005
  true && false                                   //> res4: Boolean(false) = false

  // prop.p006
  true && true                                    //> res5: Boolean(true) = true

  // pg. 33
  // ------------------------------------------------------------------------- //
  // Example of use.                                                           //
  // ------------------------------------------------------------------------- //

  // prop.p007
  // Harrison #01
  eval("""p /\ q ==> q /\ r""")(_ match {
    case "p" => true
    case "q" => false
    case "r" => true
  })                                              //> res6: Boolean = true

  // prop.p008
  // Harrison #01
  eval("""p /\ q ==> q /\ r""")(_ match {
    case "p" => true
    case "q" => true
    case "r" => false
  })                                              //> res7: Boolean = false

  // pg. 35
  // ------------------------------------------------------------------------- //
  // Example.                                                                  //
  // ------------------------------------------------------------------------- //

  // prop.p009
  atoms("""p /\ q \/ s ==> ~p \/ (r <=> s)""")    //> res8: List[String] = List(p, q, r, s)

  // pg. 36
  // ------------------------------------------------------------------------- //
  // Example.                                                                  //
  // ------------------------------------------------------------------------- //

  // prop.p010
  // Harrison #01
  print_truthtable("""p /\ q ==> q /\ r""")       //> p      q      r      | formula
                                                  //| ------------------------------
                                                  //| false  false  false  | true   
                                                  //| false  false  true   | true   
                                                  //| false  true   false  | true   
                                                  //| false  true   true   | true   
                                                  //| true   false  false  | true   
                                                  //| true   false  true   | true   
                                                  //| true   true   false  | false  
                                                  //| true   true   true   | true   
                                                  //| ------------------------------
  // prop.p011
  // Harrison #01
  val fm002 = """p /\ q ==> q /\ r"""             //> fm002  : String = p /\ q ==> q /\ r
  print_truthtable(fm002)                         //> p      q      r      | formula
                                                  //| ------------------------------
                                                  //| false  false  false  | true   
                                                  //| false  false  true   | true   
                                                  //| false  true   false  | true   
                                                  //| false  true   true   | true   
                                                  //| true   false  false  | true   
                                                  //| true   false  true   | true   
                                                  //| true   true   false  | false  
                                                  //| true   true   true   | true   
                                                  //| ------------------------------
  // pg. 39
  // ------------------------------------------------------------------------- //
  // Additional examples illustrating formula classes.                         //
  // ------------------------------------------------------------------------- //

  // prop.p012
  // Pelletier #08
  print_truthtable("""((p ==> q) ==> p) ==> p""") //> p      q      | formula
                                                  //| -----------------------
                                                  //| false  false  | true   
                                                  //| false  true   | true   
                                                  //| true   false  | true   
                                                  //| true   true   | true   
                                                  //| -----------------------
  // prop.p013
  print_truthtable("""p /\ ~p""")                 //> p      | formula
                                                  //| ----------------
                                                  //| false  | false  
                                                  //| true   | false  
                                                  //| ----------------
  // pg. 41
  // ------------------------------------------------------------------------- //
  // Examples.                                                                 //
  // ------------------------------------------------------------------------- //

  // prop.p014
  // Pelletier #06
  tautology("""p \/ ~p"""")                       //> res9: Boolean = true

  // prop.p015
  tautology("""p \/ q ==> p"""")                  //> res10: Boolean = false
  // prop.p016
  tautology("""p \/ q ==> q \/ (p <=> q)"""")     //> res11: Boolean = false

  // prop.p017
  tautology("""(p \/ q) /\ ~(p /\ q) ==> (~p <=> q)"""")
                                                  //> res12: Boolean = true
  // pg. 43
  // ------------------------------------------------------------------------- //
  // Surprising tautologies including Dijkstra's "Golden rule".                //
  // ------------------------------------------------------------------------- //

  // prop.p019
  // Pelletier #16
  tautology("""(p ==> q) \/ (q ==> p)""")         //> res13: Boolean = true

  // prop.p020
  tautology("""p \/ (q <=> r) <=> (p \/ q <=> p \/ r)""")
                                                  //> res14: Boolean = true

  // prop.p021
  // Harrison #02 - Equations within equations
  tautology("""p /\ q <=> ((p <=> q) <=> p \/ q)""")
                                                  //> res15: Boolean = true

  // prop.p022
  // Harrison #03 - Equations within equations
  tautology("""(p ==> q) <=> (~q ==> ~p)""")      //> res16: Boolean = true

  // prop.p023
  tautology("""(p ==> ~q) <=> (q ==> ~p)""")      //> res17: Boolean = true

  // prop.p024
  tautology("""(p ==> q) <=> (q ==> p)""")        //> res18: Boolean = false

  // pg. 47
  // ------------------------------------------------------------------------- //
  // Some logical equivalences allowing elimination of connectives.            //
  // ------------------------------------------------------------------------- //

  // prop.p025

  (parse_prop_formula("""true <=> false ==> false""") ::
    parse_prop_formula("""~p <=> p ==> false""") ::
    parse_prop_formula("""p /\ q <=> (p ==> q ==> false) ==> false""") ::
    parse_prop_formula("""p \/ q <=> (p ==> false) ==> q""") ::
    parse_prop_formula("""(p <=> q) <=> ((p ==> q) ==> (q ==> p) ==> false) ==> false""")
    :: Nil).forall(tautology)                     //> res19: Boolean = true

  // pg. 49.
  // ------------------------------------------------------------------------- //
  // Example.                                                                  //
  // ------------------------------------------------------------------------- //

  // prop.p026
  // Pelletier #06
  dual("""p \/ ~p""")                             //> res20: org.edla.port.atp.Formulas.Formula = ( p /\ ~p )

  // pg. 51
  // ------------------------------------------------------------------------- //
  // Example.                                                                  //
  // ------------------------------------------------------------------------- //

  // prop.p027
  psimplify("""(true ==> (x <=> false)) ==> ~(y \/ false /\ z)""")
                                                  //> res21: org.edla.port.atp.Formulas.Formula = ~x ==> ~y
  // prop.p028
  psimplify("""((x ==> y) ==> true) \/ ~false""") //> res22: org.edla.port.atp.Formulas.Formula = true

  // pg. 53
  // ------------------------------------------------------------------------- //
  // Example of NNF function in action.                                        //
  // ------------------------------------------------------------------------- //

  val fm003: Formula = """(p <=> q) <=> ~(r ==> s)"""
                                                  //> fm003  : org.edla.port.atp.Formulas.Formula = p <=> q <=> ~r ==> s
  val fm003_ = nnf(fm003)                         //> fm003_  : org.edla.port.atp.Formulas.Formula = ( ( ( ( p /\ q ) \/ ( ~p /\ 
                                                  //| ~q ) ) /\ ( r /\ ~s ) ) \/ ( ( ( p /\ ~q ) \/ ( ~p /\ q ) ) /\ ( ~r \/ s ) 
                                                  //| ) )
  // prop.p029
  tautology(Iff(fm003, fm003_))                   //> res23: Boolean = true

  // pg. 54
  // ------------------------------------------------------------------------- //
  // Some tautologies remarked on.                                             //
  // ------------------------------------------------------------------------- //

  // prop.p030
  tautology("""(p ==> p') /\ (q ==> q') ==> (p /\ q ==> p' /\ q')""")
                                                  //> res24: Boolean = true
  // prop.p031
  tautology("""(p ==> p') /\ (q ==> q') ==> (p \/ q ==> p' \/ q')""")
                                                  //> res25: Boolean = true
  // pg. 58
  // ------------------------------------------------------------------------- //
  // Example.                                                                  //
  // ------------------------------------------------------------------------- //
  //

  // prop.p035
  // Harrison #04
  rawdnf("""(p \/ q /\ r) /\ (~p \/ ~r)""")       //> res26: org.edla.port.atp.Formulas.Formula = ( ( ( p /\ ~p ) \/ ( ( q /\ r )
                                                  //|  /\ ~p ) ) \/ ( ( p /\ ~r ) \/ ( ( q /\ r ) /\ ~r ) ) )
  // prop.p036
  // Harrison #04
  purednf("""(p \/ q /\ r) /\ (~p \/ ~r)""")      //> res27: List[List[org.edla.port.atp.Formulas.Formula]] = List(List(p, ~p), L
                                                  //| ist(p, ~r), List(q, r, ~p), List(q, r, ~r))
  // pg. 59
  // ------------------------------------------------------------------------- //
  // Example.                                                                  //
  // ------------------------------------------------------------------------- //

  // prop.p037
  // Harrison #04

  purednf("""(p \/ q /\ r) /\ (~p \/ ~r)""").filter(!trivial(_))
                                                  //> res28: List[List[org.edla.port.atp.Formulas.Formula]] = List(List(p, ~r), L
                                                  //| ist(q, r, ~p))
}