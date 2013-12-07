package org.gilbertlang
package gparser
package testing

import gparser.GParser
import ast.GAst
import org.scalatest.Assertions
import org.junit.Test
import scala.util.parsing.input.StreamReader
import java.io.FileReader
import glibrary.Operators._
import java.io.InputStreamReader
import misc.PrettyPrinter

class MParserTest extends GParser with Assertions {

  import GAst._
  import lexer.EOF

  @Test def testMParser {
    val expected = ASTProgram(List(ASTAssignment(ASTIdentifier("A"), ASTFunctionApplication(ASTIdentifier("load"),
      List(ASTString("inputfile"), ASTInteger(10), ASTInteger(10)))), ASTAssignment(ASTIdentifier("B"),
      ASTFunctionApplication(ASTIdentifier("bin"), List(ASTIdentifier("A")))), ASTAssignment(ASTIdentifier("C"),
      ASTBinaryExpression(ASTUnaryExpression(ASTIdentifier("B"), TransposeOp), MultOp, ASTIdentifier("B"))),
      ASTOutputResultStatement(ASTAssignment(ASTIdentifier("D"), ASTBinaryExpression(ASTIdentifier("C"),
        CellwiseDivOp, ASTFunctionApplication(ASTIdentifier("maxValue"), List(ASTIdentifier("C"))))))))

    val inputURL = ClassLoader.getSystemResource("input.m");
    val inputReader = StreamReader(new FileReader(inputURL.toURI().getPath()));

    val ast = phrase(program)(inputReader)

    ast match {
      case Success(value, in) =>
        assert(in.atEnd); expectResult(expected)(value)
      case _ => fail()
    }
  }

  @Test def testFunctionDefinitions {
    val inputReader = StreamReader(new InputStreamReader(ClassLoader.getSystemResourceAsStream("function.m")))

    val expected = ASTProgram(List(ASTFunction(List(ASTIdentifier("X")), ASTIdentifier("foobar"),
      List(ASTIdentifier("Y"), ASTIdentifier("Z")), ASTProgram(List(ASTAssignment(ASTIdentifier("A"),
        ASTBinaryExpression(ASTIdentifier("Y"), MultOp, ASTIdentifier("Z"))), ASTOutputResultStatement(
        ASTAssignment(ASTIdentifier("X"), ASTIdentifier("A"))))))))
    val ast = phrase(program)(inputReader)

    import gparser.misc

    ast match {
      case Success(program: ASTProgram, _) =>
        expectResult(expected)(program)
      case _ => fail("Could not parse function definition")
    }

  }

  @Test def testFunctionParameters {
    val input = """(X,Y,Z)""";
    val expected = List(ASTIdentifier("X"), ASTIdentifier("Y"), ASTIdentifier("Z"))

    functionParams(input) match {
      case Success(result, in) => {
        assert(in.first == EOF)
        expectResult(expected)(result)
      }
      case _ => fail()
    }
  }

  @Test def testIdentifierList {
    val input = """X,Y,Z""";
    val expected = List(ASTIdentifier("X"), ASTIdentifier("Y"), ASTIdentifier("Z"))

    identifierList(input) match {
      case Success(result, in) => {
        assert(in.first == EOF)
        expectResult(expected)(result)
      }
      case _ => fail()
    }

  }

  @Test def testAnonymousFunction {
    val inputReader = StreamReader(new InputStreamReader(ClassLoader.getSystemResourceAsStream("anonymousFunction.m")))
    val expected = ASTProgram(List(
      ASTOutputResultStatement(
        ASTAssignment(
          ASTIdentifier("X"),
          ASTAnonymousFunction(List(
            ASTIdentifier("A"),
            ASTIdentifier("B")),
            ASTBinaryExpression(
              ASTIdentifier("A"),
              PlusOp,
              ASTIdentifier("B")))))))

    phrase(program)(inputReader) match {
      case Success(actual, _) => {
        expectResult(expected)(actual)
      }
      case _ => fail("Could not parse anonymousFunction.m")
    }
  }

  @Test def testFunctionReference {
    val inputReader = StreamReader(new InputStreamReader(ClassLoader.getSystemResourceAsStream("functionReference.m")))
    val expected = ASTProgram(List(
      ASTAssignment(
        ASTIdentifier("X"),
        ASTFunctionReference(
          ASTIdentifier("foobar")))))
          
   phrase(program)(inputReader) match{
      case Success(actual,_) =>{
        expectResult(expected)(actual)
      }
      case _ => fail("Could not parse functionReference.m")
    }
          
  }
}
