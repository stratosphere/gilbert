package de.tuberlin.dima.stratosphere.gilbert.mparser

import scala.util.parsing.input.StreamReader
import java.io.FileReader
import de.tuberlin.dima.stratosphere.gilbert.mparser.misc.PrettyPrinter

object App extends MParser {
  def main(args:Array[String]) = {
	val inputURL = ClassLoader.getSystemResource("input.m");
    val inputReader = StreamReader(new FileReader(inputURL.toURI().getPath()));
    
    val ast = phrase(program)(inputReader)
    
    println(ast)
    
    ast match {
      case Success(program,in) => PrettyPrinter.printProgram(program, 0)
      case f => println("Could not parse program: " + f)
    }
    
  }
}