package org.gilbertlang.mtyper.testing

import org.scalatest.Assertions
import org.junit.Test
import org.gilbertlang.mparser.ast.MAst._
import org.gilbertlang.mtyper.MTyper
import org.gilbertlang.mtyper.types.MTypedAst._
import org.gilbertlang.mlibrary.MTypes._
import org.gilbertlang.mlibrary.MValues._
import org.gilbertlang.mlibrary.MTypes.Helper._
import org.gilbertlang.mlibrary.MValues.Helper._
import org.gilbertlang.mparser.MParser
import scala.util.parsing.input.StreamReader
import java.io.InputStreamReader
import org.gilbertlang.mtyper.misc.PrettyPrinter

class MTyperTest extends Assertions{
  
  @Test def testProgram{
    val ast = ASTProgram(List(ASTAssignment(ASTIdentifier("x"),ASTInteger(12))))
    val expected = TypedProgram(List(TypedAssignment(TypedIdentifier("x",IntegerType),TypedInteger(12))))
    val typer = new MTyper{}
    val result = typer.typeProgram(ast)
    
    expectResult(expected)(result)
  }
  
  @Test def testCharacterIntegerUnification{
    val typer = new MTyper{}
    
    expectResult(Some(IntegerType))(typer.unify(CharacterType, IntegerType))
  }
  
  @Test def testMatrixMatrixUnification1{
    val typer = new MTyper{}
    
    expectResult(Some(MatrixType(IntegerType,IntValue(10),IntValue(42))))(typer.unify(MatrixType(newTV(),newVV(),IntValue(42)),
        MatrixType(IntegerType,IntValue(10),newVV())))
  }
  
  @Test def testFunctionTyping{
    val typer = new MTyper{}
    val parser = new MParser{}
    
    val inputReader = StreamReader(new InputStreamReader(getClass().getClassLoader().getResourceAsStream("function.m")))
    
    parser.parse(inputReader) match {
      case Some(ast) =>
        val typedAST = typer.typeProgram(ast)
        PrettyPrinter.printProgram(typedAST,0)
      case _ => fail("Could not parse input file")
    }
    
  }

}