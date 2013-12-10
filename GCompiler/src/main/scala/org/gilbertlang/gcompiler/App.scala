package org.gilbertlang.gcompiler

import scala.util.parsing.input.StreamReader
import java.io.InputStreamReader
import org.gilbertlang.gparser.GParser
import org.gilbertlang.gtyper.GTyper
import org.gilbertlang.shell.local

/**
 * @author ${user.name}
 */
object App extends GParser {
  def main(args: Array[String]) {
    val inputReader = StreamReader(new InputStreamReader(ClassLoader.getSystemResourceAsStream("pageRank.m")))
    val typer = new GTyper {}
    val compiler = new GCompiler {}

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
