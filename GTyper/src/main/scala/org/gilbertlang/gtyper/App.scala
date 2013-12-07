package org.gilbertlang
package gtyper

import scala.util.parsing.input.StreamReader
import java.io.FileReader
import misc.Printer
import misc.VerbosePrinter
import gparser.GParser


/**
 * @author ${user.name}
 */
object App extends GParser {
  def main(args: Array[String]) {
    val typer = new GTyper{}
//    val input = """ A = load("inputfile",10,2);
//    				B = binarize(A);
//    				C = B' * B;
//    				D = C ./ maxValue(C)
//      """
      
    val inputURL = ClassLoader.getSystemResource("pageRank.m");
    val inputReader = StreamReader(new FileReader(inputURL.toURI().getPath()));
    val parseResult = program(inputReader)
    
    parseResult match{
      case Success(program,rest) => 
        val typedProgram = typer.typeProgram(program)
        Printer.print(typedProgram)
      case _ => println("Could not type program")
    }
    
    
  }

}
