package de.tuberlin.dima.stratosphere.gilbert.mlexer.testing

import org.scalatest.Assertions
import de.tuberlin.dima.stratosphere.gilbert.mlexer.MLexer
import de.tuberlin.dima.stratosphere.gilbert.mlexer.token.DiscardWhitespaces
import java.io.FileReader
import scala.util.parsing.input.StreamReader
import de.tuberlin.dima.stratosphere.gilbert.mlexer.token.MTokens
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.junit.Test
import org.scalatest.Spec

class LexerTest extends MLexer with DiscardWhitespaces with Assertions {
  
  @Test def testMLexer() {
    val expected = List(Identifier("A"),Keyword("="),Identifier("load"),Keyword("("),
        StringLiteral("path to file"),Keyword(","),IntegerLiteral(10),Keyword(","),
        IntegerLiteral(10),Keyword(")"),Keyword("\n"),Identifier("B"),Keyword("="),Identifier("bin"),
        Keyword("("),Identifier("A"),Keyword(")"),Keyword("\n"),Identifier("C"),Keyword("="),
        Identifier("B"),Keyword("\'"),Keyword("*"),Identifier("B"),Keyword("\n"),Identifier("D"),
        Keyword("="),Identifier("C"),Keyword("./"),Identifier("maxValue"),Keyword("("),Identifier("C"),
        Keyword(")"),Keyword("\n"),Keyword("\n"))
    val lexer = new MLexer with DiscardWhitespaces
    
    val inputFileURL = ClassLoader.getSystemResource("input.m");
    
    val fileReader = new FileReader(inputFileURL.toURI.getPath())
    val streamReader = StreamReader(fileReader)
    
    val result = lex(streamReader)
     
    expected.zip(result).foreach{ case (e,r) => expectResult(e)(r)}
  }
}