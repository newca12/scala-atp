import org.edla.port.atp.Prop._
import org.edla.study.parsing.parboiled.AST._

object prop {
  // pg. 29
  // ------------------------------------------------------------------------- //
  // Testing the parser and printer.                                           //
  // ------------------------------------------------------------------------- //

  val fm001 = parse_prop_formula("""p ==> q <=> r /\ s \/ (t <=> ~ ~u /\ v)""")
                                                  //> fm001  : org.edla.study.parsing.parboiled.AST.Expr = Equiv(Impl(p,q),And(r,s
                                                  //| ))
  // pg. 30
  // prop.p001
  And(fm001, fm001)                               //> res0: org.edla.study.parsing.parboiled.AST.And = And(Equiv(Impl(p,q),And(r,s
                                                  //| )),Equiv(Impl(p,q),And(r,s)))
  // prop.p002
  And(Or(fm001, fm001), fm001)                    //> res1: org.edla.study.parsing.parboiled.AST.And = And(Or(Equiv(Impl(p,q),And(
                                                  //| r,s)),Equiv(Impl(p,q),And(r,s))),Equiv(Impl(p,q),And(r,s)))

  // pg. 33
  // prop.p003
  false && false                                  //> res2: Boolean(false) = false

  // prop.p004
  false && true                                   //> res3: Boolean(false) = false

  // prop.p005
  true && false                                   //> res4: Boolean(false) = false

  // prop.p006
  true && true                                    //> res5: Boolean(true) = true
  
  //eval(parse_prop_formula("""p /\ q ==> q /\ r"""))
}