import at.logic.language.fol._
import at.logic.language.lambda.types.{->, To, Ti}
import at.logic.language.lambda.typedLambdaCalculus._
import at.logic.language.lambda.symbols._
import at.logic.language.hol.logicSymbols._

object PlayGround {
 
val x :FOLTerm = FOLVar(VariableStringSymbol("x"))//> x  : at.logic.language.fol.FOLTerm = x
val one : FOLTerm = FOLConst(ConstantStringSymbol("1"))
                                                  //> one  : at.logic.language.fol.FOLTerm = 1
val f : FOLTerm = Function(ConstantStringSymbol("+"), List(x, one))
                                                  //> f  : at.logic.language.fol.FOLTerm = +(x, 1)

}