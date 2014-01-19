// ========================================================================= //
// Copyright (c) 2003-2007, John Harrison.                                   //
// Copyright (c) 2012-2014, Olivier ROLAND.                                  //
// (See "LICENSE.txt" for details.)                                          //
// ========================================================================= //

import org.edla.port.atp.Prop._
import org.edla.study.parsing.parboiled.AST._

object prop {

  implicit def default_parser( s: String ) = parse_prop_formula(s)
                                                  //> default_parser: (s: String)org.edla.study.parsing.parboiled.AST.Formula
  // pg. 29
  // ------------------------------------------------------------------------- //
  // Testing the parser (and [pretty] printer not yet implemented) 						 //
  // ------------------------------------------------------------------------- //
  
  // nota: ~ ~u is not allowed
  val fm001: Formula = """p ==> q <=> r /\ s \/ (t <=> ~(~u) /\ v)"""
                                                  //> fm001  : org.edla.study.parsing.parboiled.AST.Formula = Iff(Imp(p,q),Or(And(
                                                  //| r,s),Iff(t,And(Not(Not(u)),v))))
  // pg. 30
  // prop.p001
  And(fm001, fm001)                               //> res0: org.edla.study.parsing.parboiled.AST.And = And(Iff(Imp(p,q),Or(And(r,s
                                                  //| ),Iff(t,And(Not(Not(u)),v)))),Iff(Imp(p,q),Or(And(r,s),Iff(t,And(Not(Not(u))
                                                  //| ,v)))))
  // prop.p002
  And(Or(fm001, fm001), fm001)                    //> res1: org.edla.study.parsing.parboiled.AST.And = And(Or(Iff(Imp(p,q),Or(And
                                                  //| (r,s),Iff(t,And(Not(Not(u)),v)))),Iff(Imp(p,q),Or(And(r,s),Iff(t,And(Not(No
                                                  //| t(u)),v))))),Iff(Imp(p,q),Or(And(r,s),Iff(t,And(Not(Not(u)),v)))))
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
  eval("""p /\ q ==> q /\ r""")( _ match {
    case "p" => true
    case "q" => false
    case "r" => true
  })                                              //> res6: Boolean = true

 // prop.p008
 // Harrison #01
  eval("""p /\ q ==> q /\ r""")( _ match {
    case "p" => true
    case "q" => true
    case "r" => false
  })                                              //> res7: Boolean = false
  
 // pg. 35
 // ------------------------------------------------------------------------- //
 // Example.                                                                  //
 // ------------------------------------------------------------------------- //

 // prop.p009
 atoms("""p /\ q \/ s ==> ~p \/ (r <=> s)""")     //> res8: List[String] = List(p, q, r, s)
 
 // pg. 36
 // ------------------------------------------------------------------------- //
 // Example.                                                                  //
 // ------------------------------------------------------------------------- //

 // prop.p010
 // Harrison #01
 print_truthtable("""p /\ q ==> q /\ r""")        //> p      q      r      | formula
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
 val fm002 = """p /\ q ==> q /\ r"""              //> fm002  : String = p /\ q ==> q /\ r
 print_truthtable(fm002)                          //> p      q      r      | formula
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
 print_truthtable("""((p ==> q) ==> p) ==> p""")  //> p      q      | formula
                                                  //| -----------------------
                                                  //| false  false  | true   
                                                  //| false  true   | true   
                                                  //| true   false  | true   
                                                  //| true   true   | true   
                                                  //| -----------------------
 // prop.p013
print_truthtable ("""p /\ ~p""")                  //> p      | formula
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
 tautology("""p \/ ~p"""")                        //> res9: Boolean = true
 
 // prop.p015
 tautology("""p \/ q ==> p"""")                   //> res10: Boolean = false
 // prop.p016
 tautology("""p \/ q ==> q \/ (p <=> q)"""")      //> res11: Boolean = false

 // prop.p017
 tautology("""(p \/ q) /\ ~(p /\ q) ==> (~p <=> q)"""")
                                                  //> res12: Boolean = true
                                                  
}