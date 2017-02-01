//https://gist.github.com/drXor/e0d55894d788396d5141
package org.edla.study.lambda_calculus

/**
  * This is a full implementation of the lambda calculus in Scala's type system,
  * where
  * <ul>
  * <li> `->:`    is lambda abstraction
  * <li> `@@`     is application
  * <li> `a..z`   can be used as variables
  * <li> `eval[...]` evaluates a term
  * </ul>
  * <br>
  * Included are various useful objects and functions, such as
  * Church numerals, booleans, arithmetic, logic, and the Y
  * combinator.
  * <br>
  * Due to restrictions on the JVM's computing power, the actual things computable are
  * limited. However, we obtain Turing completeness when these restrictions are lifted.
  * <br>
  *
  * @author Xor Boole
  */
package object lambda {

  import language.higherKinds

  type None = Nothing

  /**
    * Base type for lambda terms.
    * <br>
    * Member types are treated as "methods" taking types as arguments and returning other types.
    * For example, to ap a term `f` to a term `x`, we write `f#ap[x]`, using type projection as
    * a sort of selection operand, and type construction to pass arguments.
    */
  trait Term {

    /**
      * Perform a substitution on this term
      * @tparam v the variable being substituted
      * @tparam u the term being substituted for
      */
    type sub[v <: Var, u <: Term] <: Term

    /**
      * Perform an application.
      * @tparam u the term this term is applied to
      */
    type ap[u <: Term] <: Term

    /**
      * Evaluate this term.
      * <br>
      * This "method" will attempt to reduce the
      * lambda term as best it can.
      */
    type eval <: Term
  }

  /**
    * Base type for variable symbols
    *
    * Variables are defined similarly to Peano numbers.
    */
  trait Var extends Term {
    type eval    = this.type
    type resolve = this.type

    private[lambda] type Match[ifCons[_ <: Var] <: Up, ifNil <: Up, Up] <: Up
    private[lambda] type Compare[v <: Var] <: choice.Comparison
  }

  trait VNil extends Var {
    type ap[u <: Term]            = Ap[VNil, u#eval]
    type sub[v <: Var, u <: Term] = choice.BoolGet[choice.VarEq[this.type, v], u, this.type]#get

    private[lambda] type Match[ifCons[_ <: Var] <: Up, ifNil <: Up, Up] = ifNil
    private[lambda] type Compare[v <: Var]                              = v#Match[choice.ConstNE, choice.EQ, choice.Comparison]
  }

  trait VCons[prev <: Var] extends Var {
    type ap[u <: Term]            = Ap[VCons[prev], u#eval]
    type sub[v <: Var, u <: Term] = choice.BoolGet[choice.VarEq[this.type, v], u, this.type]#get

    private[lambda] type Match[ifCons[_ <: Var] <: Up, ifNil <: Up, Up] = ifCons[prev]
    private[lambda] type Compare[v <: Var]                              = v#Match[prev#Compare, choice.NE, choice.Comparison]
  }

  /**
    * Used for comparing Var type instances
    */
  private[lambda] object choice {

    type VarEq[x <: Var, y <: Var] = x#Compare[y]#eq
    trait BoolGet[p <: Bool, a <: Term, b <: Term] {
      type get = p#get[a, b, Term]
    }

    trait Bool {
      type get[a <: T, b <: T, T] <: T
    }
    trait True extends Bool {
      type get[a <: T, b <: T, T] = a
    }
    trait False extends Bool {
      type get[a <: T, b <: T, T] = b
    }
    trait Comparison {
      type Match[IfEQ <: Up, IfNE <: Up, Up] <: Up

      type eq = Match[True, False, Bool]
    }
    trait EQ extends Comparison {
      type Match[IfEQ <: Up, IfNE <: Up, Up] = IfEQ
    }
    trait NE extends Comparison {
      type Match[IfEQ <: Up, IfNE <: Up, Up] = IfNE
    }

    type ConstNE[_] = choice.NE
  }

  /**
    * An application.
    * @tparam f the function being applied.
    * @tparam x the term the function is applied to.
    */
  trait Ap[f <: Term, x <: Term] extends Term {
    type sub[v <: Var, u <: Term] = Ap[f#sub[v, u], x#sub[v, u]]
    type ap[u <: Term]            = Ap[Ap[f, x], u]
    type eval                     = f#eval#ap[x]
  }

  /**
    * An abstraction
    * @tparam arg the variable for the abstraction
    * @tparam body the body of the abstraction.
    */
  trait Lam[arg <: Var, body <: Term] extends Term {
    type sub[v <: Var, u <: Term] =
      choice.BoolGet[
        choice.VarEq[arg, v],
        Lam[arg, body],
        Lam[arg, body#sub[v, u]]
      ]#get
    type ap[u <: Term] = body#sub[arg, u]#eval
    type eval          = Lam[arg, body#eval]
  }

  /* * Common functions and aliases * */

  type x = VNil
  type y = VCons[x]
  type z = VCons[y]

  type a = VCons[z]
  type b = VCons[a]
  type c = VCons[b]
  type d = VCons[c]
  type e = VCons[d]
  type f = VCons[e]
  type g = VCons[f]
  type h = VCons[g]
  type i = VCons[h]
  type j = VCons[i]
  type k = VCons[j]
  type l = VCons[k]
  type m = VCons[l]
  type n = VCons[m]
  type o = VCons[n]
  type p = VCons[o]
  type q = VCons[p]
  type r = VCons[q]
  type s = VCons[r]
  type t = VCons[s]
  type u = VCons[t]
  type v = VCons[u]
  type w = VCons[v]

  type @@[f <: Term, arg <: Term]     = Ap[f, arg]
  type ->:[args <: Var, body <: Term] = Lam[args, body]

  type toAB = f ->: a ->: b ->: (f @@ a @@ b)
  type toXY = f ->: x ->: y ->: (f @@ x @@ y)

  type T = x ->: y ->: x
  type F = x ->: y ->: y

  type And = a ->: b ->: (a @@ b @@ a)
  type Or  = a ->: b ->: (a @@ a @@ b)
  type Not = a ->: b ->: c ->: (a @@ c @@ b)
  type If  = a ->: b ->: c ->: (a @@ b @@ c)

  type &&[p <: Term, q <: Term] = And#ap[p]#ap[q]
  type ||[p <: Term, q <: Term] = Or#ap[p]#ap[q]
  type ![p <: Term]             = toXY#ap[Not#ap[p]]

  type _0 = F

  type Succ          = n ->: x ->: y ->: (x @@ (n @@ x @@ y))
  type ++[n <: Term] = Succ#ap[n]

  type _1 = ++[_0]
  type _2 = ++[_1]
  type _3 = ++[_2]
  type _4 = ++[_3]
  type _5 = ++[_4]

  type Plus                    = m ->: n ->: x ->: y ->: (m @@ x @@ (n @@ x @@ y))
  type +[m <: Term, n <: Term] = Plus#ap[m]#ap[n]

  type Mult                    = m ->: n ->: x ->: (m @@ (n @@ x))
  type *[m <: Term, n <: Term] = Mult#ap[m]#ap[n]

  type Exp                      = b ->: e ->: (e @@ b)
  type **[b <: Term, e <: Term] = toXY#ap[Exp#ap[toAB#ap[b]]#ap[e]]

  type IsZero            = n ->: (n @@ (y ->: F) @@ T)
  type isZero[n <: Term] = IsZero#ap[n]#eval

  type Pred          = n ->: (n @@ (g ->: k ->: (IsZero @@ (g @@ _1) @@ k @@ (Plus @@ (g @@ k) @@ _1))) @@ (v ->: _0) @@ _0)
  type --[n <: Term] = Pred#ap[n]#eval#eval

  type Sub                     = m ->: n ->: (n @@ Pred @@ m)
  type -[m <: Term, n <: Term] = Sub#ap[m]#ap[n]

  type Leq                      = m ->: n ->: (IsZero @@ (Sub @@ m @@ n))
  type <=[m <: Term, n <: Term] = Leq#ap[m]#ap[n]

  type Pair                     = x ->: y ->: f ->: (f @@ x @@ y)
  type ~~[x <: Term, y <: Term] = Pair#ap[x]#ap[y]

  type First  = p ->: (p @@ T)
  type Second = p ->: (p @@ F)
  type Nil    = x ->: T
  type Empty  = p ->: (p @@ (x ->: y ->: F))

  type Y = g ->: ((x ->: (g @@ (x @@ x))) @@ (x ->: (g @@ (x @@ x))))

  type Fact = r ->: n ->: (If @@ (IsZero @@ n) @@ _1 @@ (Mult @@ n @@ (r @@ r @@ (Sub @@ n @@ _1))))

  import reflect.runtime.{universe => ru}

  def eval[u <: Term: ru.WeakTypeTag](implicit u: ru.WeakTypeTag[u#eval]): String = eval[u]()

  /**
    * Pretty-prints a lambda term.
    * @param fancy whether ot use the `λx.y` style or the `x ->: y` style.
    * @tparam u a lambda term.
    * @return A pretty-printed version of the term.
    */
  def eval[u <: Term: ru.WeakTypeTag](fancy: Boolean = true)(implicit u: ru.WeakTypeTag[u#eval] // yep
  ): String = {
    import ru._

    def pretty(tpe: Type): String = tpe.dealias match {
      case TypeRef(_, sym, List(arg, body)) if sym.name.toString == "Lam" =>
        if (fancy) s"(λ${pretty(arg)}.${pretty(body)})"
        else s"(${pretty(arg)} ->: ${pretty(body)})"
      case TypeRef(_, sym, List(f, x)) if sym.name.toString == "Ap" =>
        if (fancy) s"(${pretty(f)} ${pretty(x)})"
        else s"(${pretty(f)} @@ ${pretty(x)})"
      case TypeRef(_, sym, List(prev)) if sym.name.toString == "VCons" =>
        val point = pretty(prev)(0)
        val next = point match {
          case 'z' => 'a'
          case 'w' => 'A'
          case c   => (c + 1).toChar
        }
        next.toString
      case TypeRef(_, sym, Nil) if sym.name.toString == "VNil" => "x"
    }

    pretty(u.tpe)
  }
}
