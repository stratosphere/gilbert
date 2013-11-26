package de.tuberlin.dima.stratosphere.gilbert.mcompiler

import scala.util.parsing.input.StreamReader
import java.io.InputStreamReader
import de.tuberlin.dima.stratosphere.gilbert.mparser.MParser
import de.tuberlin.dima.stratosphere.gilbert.mtyper.MTyper
import nio.ssc.gilbert.shell.local

/**
 * @author ${user.name}
 */
object App extends MParser {
  def main(args: Array[String]) {
    val inputReader = StreamReader(new InputStreamReader(ClassLoader.getSystemResourceAsStream("input.m")))
    val typer = new MTyper {}
    val compiler = new MCompiler {}

    val ast = phrase(program)(inputReader)

    ast match {
      case Success(parsedProgram, _) =>
        val typedAST = typer.typeProgram(parsedProgram)
        val compiledProgram = compiler.compile(typedAST)

        local(compiledProgram)

      case _ => println("Could not parse program")
    }

  }

}
