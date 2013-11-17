package de.tuberlin.dima.stratosphere.gilbert.mparser.testing

import de.tuberlin.dima.stratosphere.gilbert.mparser.MParser
import de.tuberlin.dima.stratosphere.gilbert.mparser.ast.MAst
import org.scalatest.Assertions
import org.junit.Test
import scala.util.parsing.input.StreamReader
import java.io.FileReader

class MParserTest extends MParser with Assertions {

  import MAst._
  
  @Test def testMParser{
    val expected = ASTProgram(List(ASTAssignment(ASTIdentifier("A"),ASTFunctionApplication(ASTIdentifier("load"),List(ASTString("inputfile"), ASTInteger(10), ASTInteger(10)))), ASTAssignment(ASTIdentifier("B"),ASTFunctionApplication(ASTIdentifier("bin"),List(ASTIdentifier("A")))), ASTAssignment(ASTIdentifier("C"),ASTBinaryExpression(ASTUnaryExpression(ASTIdentifier("B"),ASTTranspose),ASTMult,ASTIdentifier("B"))), ASTAssignment(ASTIdentifier("D"),ASTBinaryExpression(ASTIdentifier("C"),ASTCellwiseDiv,ASTFunctionApplication(ASTIdentifier("maxValue"),List(ASTIdentifier("C")))))))
    
    val inputURL = ClassLoader.getSystemResource("input.m");
    val inputReader = StreamReader(new FileReader(inputURL.toURI().getPath()));
    
    val ast = program(inputReader)
    
    val a = ASTProgram(List(ASTIdentifier("A"),ASTIdentifier("load")))
    val b = ASTProgram(List(ASTIdentifier("A"),ASTIdentifier("load")))
    
    ast match {
      case Success(value,in) => assert(in.atEnd); expectResult(expected)(value)
      case _ => fail()
    }
  }

}
