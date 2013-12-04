package de.tuberlin.dima.stratosphere.gilbert.mtyper.testing

import org.scalatest.Assertions
import org.junit.Test
import de.tuberlin.dima.stratosphere.gilbert.mparser.ast.MAst._
import de.tuberlin.dima.stratosphere.gilbert.mtyper.MTyper
import de.tuberlin.dima.stratosphere.gilbert.mtyper.types.MTypedAst._
import de.tuberlin.dima.stratosphere.gilbert.mtyper.types.MTypes._
import de.tuberlin.dima.stratosphere.gilbert.mtyper.types.MValues._
import de.tuberlin.dima.stratosphere.gilbert.mtyper.types.MTypes.Helper._
import de.tuberlin.dima.stratosphere.gilbert.mtyper.types.MValues.Helper._

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

}