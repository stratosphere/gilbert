package de.tuberlin.dima.stratosphere.gilbert.mtyper

import de.tuberlin.dima.stratosphere.gilbert.mtyper.misc.PrettyPrinter._
import de.tuberlin.dima.stratosphere.gilbert.mparser.misc.PrettyPrinter
import de.tuberlin.dima.stratosphere.gilbert.mparser.MParser

/**
 * @author ${user.name}
 */
object App extends MParser {
  def main(args: Array[String]) {
    val typer = new MTyper{}
    val input = """ A = load("inputfile",10,2)
    				B = bin(A)
    				C = B' * B
    				D = C ./ maxValue(C)"""
    val parseResult = program(input)
    
    parseResult match{
      case Success(program,rest) => 
        val typedProgram = typer.typeProgram(program)
        printProgram(typedProgram,0)
      case _ => println("Could not type program:" + input)
    }
    
    
  }

}
