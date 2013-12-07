package org.gilbertlang.glexer.testing

import org.scalatest.Assertions
import org.gilbertlang.glexer.GLexer
import org.gilbertlang.glexer.token.DiscardWhitespaces
import java.io.FileReader
import scala.util.parsing.input.StreamReader
import org.gilbertlang.glexer.token.GTokens
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.junit.Test
import org.scalatest.Spec
import scala.util.parsing.input.CharSequenceReader
import scala.util.parsing.input.CharArrayReader

class LexerTest extends GLexer with DiscardWhitespaces with Assertions {
  
  @Test def testMLexer() {
    val expected = List(Identifier("A"),Keyword("="),Identifier("load"),Keyword("("),
        StringLiteral("path to file"),Keyword(","),IntegerLiteral(10),Keyword(","),
        IntegerLiteral(10),Keyword(")"),Keyword("\n"),Identifier("B"),Keyword("="),Identifier("bin"),
        Keyword("("),Identifier("A"),Keyword(")"),Keyword("\n"),Identifier("C"),Keyword("="),
        Identifier("B"),Keyword("\'"),Keyword("*"),Identifier("B"),Keyword("\n"),Identifier("D"),
        Keyword("="),Identifier("C"),Keyword("./"),Identifier("maxValue"),Keyword("("),Identifier("C"),
        Keyword(")"),Keyword("\n"),Keyword("\n"),EOF)
    val lexer = new GLexer with DiscardWhitespaces
    
    val inputFileURL = ClassLoader.getSystemResource("input.m");
    
    val fileReader = new FileReader(inputFileURL.toURI.getPath())
        
    val streamReader = StreamReader(fileReader)
    
    val result = lex(streamReader)
    
    expectResult(expected)(result)
  }
  
  @Test def testEOFChar(){
    import scala.util.parsing.input.CharArrayReader.EofCh
    val expected = List(Identifier("X"),EOF)
    val input = "X";
    
    val result = lex(input)
    
    expectResult(expected)(result)
  }
}