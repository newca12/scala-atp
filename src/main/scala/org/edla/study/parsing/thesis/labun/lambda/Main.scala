package org.edla.study.parsing.thesis.labun.lambda

object Main extends App {

  for (a <- args) {
    try { process(a) }
    catch { case e => println("unexpected error: " + e.getMessage) }
  }

  def process(input: String) {
    println("\nINPUT:\n" + input)
    (Parser parse input) match {
      case Right(ast) =>
        println("\nTREE:\n" + ast)
        println("\nEVAL:"); Interpreter eval ast
      case Left(errMsg) =>
        println(errMsg)
    }
    println("====================")
  }

}