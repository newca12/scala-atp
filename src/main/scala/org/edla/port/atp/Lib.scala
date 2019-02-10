package org.edla.port.atp

object Lib {

  def subset[A](lst1: List[A], lst2: List[A]): Boolean = {
    lst1.forall(lst2.contains)
  }

  def psubset[A](lst1: List[A], lst2: List[A]): Boolean = {
    subset(lst1, lst2) && (lst1 != lst2)
  }
}
