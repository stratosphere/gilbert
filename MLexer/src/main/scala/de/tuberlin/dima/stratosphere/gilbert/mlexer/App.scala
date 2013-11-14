package de.tuberlin.dima.stratosphere.gilbert.mlexer

import de.tuberlin.dima.stratosphere.gilbert.mlexer.token.MTokens
import de.tuberlin.dima.stratosphere.gilbert.mlexer.token.DiscardWhitespaces

/**
 * @author Till Rohrmann
 */
object App{
  
  def foo(x : Array[String]) = x.foldLeft("")((a,b) => a + b)
  
  def main(args : Array[String]) {
    val testInput = """A' * B + 12 * 12.43e-10 + print('hallo')'
      C*D""";
    
    val lexer = new MLexer with DiscardWhitespaces
    
    lexer.lex(testInput) foreach println _
  }
}
