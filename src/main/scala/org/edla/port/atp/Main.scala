package org.edla.port.atp

import org.edla.port.atp.Prop._
import org.edla.port.atp.PropExamples.ramsey

object Main extends {
  def main(args: Array[String]): Unit = {
    println("START")
    println(tautology(ramsey(args(0).toInt, args(1).toInt, args(2).toInt)))
  }
}
