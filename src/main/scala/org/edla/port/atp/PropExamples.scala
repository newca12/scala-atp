package org.edla.port.atp
import org.edla.port.atp.Formulas.{Atom, Formula, Not, Or}
import org.edla.port.atp.Prop.{set_conj, set_disj}

object PropExamples {

  def ramsey(s: Int, t: Int, n: Int): Formula = {
    val vertices                    = (1 to n).toSet
    val yesgrps: Set[Set[Set[Int]]] = vertices.subsets(s).map(_.subsets(2).toSet).toSet
    val nogrps                      = vertices.subsets(t).map(_.subsets(2).toSet).toSet
    def e(s: Set[Int]): Formula = s.toSeq match {
      case Seq(m, n) => Atom(s"p_${m}_${n}")
    }
    def ne(s: Set[Int]): Formula = Not(e(s))
    Or(set_disj(yesgrps.map(_.map(e)).map(set_conj)), set_disj(nogrps.map(_.map(ne)).map(set_conj)))
  }

}
