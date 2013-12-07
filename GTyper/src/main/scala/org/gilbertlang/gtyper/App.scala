package org.gilbertlang.mtyper

import org.gilbertlang.mparser.misc.PrettyPrinter
import org.gilbertlang.mparser.MParser
import scala.util.parsing.input.StreamReader
import java.io.FileReader
import org.gilbertlang.mtyper.misc.Printer
import org.gilbertlang.mtyper.misc.VerbosePrinter


/**
 * @author ${user.name}
 */
object App extends MParser {
  def main(args: Array[String]) {
    val typer = new MTyper{}
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
