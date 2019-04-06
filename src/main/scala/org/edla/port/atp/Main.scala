package org.edla.port.atp

import org.edla.port.atp.Prop._
import org.edla.port.atp.PropExamples.ramsey

object Main extends {

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + "ns")
    result
  }


  def main(args: Array[String]): Unit = {
    println("START")
    time {
      println(tautology(ramsey(args(0).toInt, args(1).toInt, args(2).toInt)))
    }
  }
}
