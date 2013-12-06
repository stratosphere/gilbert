package org.gilbertlang.mparser

import scala.util.parsing.input.StreamReader
import java.io.FileReader
import misc.PrettyPrinter

object App extends MParser {
  def main(args:Array[String]) = {
	val inputURL = ClassLoader.getSystemResource("pageRank.m");
    var inputReader = StreamReader(new FileReader(inputURL.toURI().getPath()));
    
    val ast = phrase(program)(inputReader)
    
    ast match {
      case Success(program,in) => PrettyPrinter.printProgram(program, 0)
      case f => println("Could not parse program: " + f)
    }
  }
}