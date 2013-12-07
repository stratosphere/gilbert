package org.gilbertlang.mtyper.testing

import org.scalatest.Assertions
import org.junit.Test
import org.gilbertlang.gparser.ast.GAst._
import org.gilbertlang.gtyper.GTyper
import org.gilbertlang.gtyper.types.GTypedAst._
import org.gilbertlang.glibrary.Types._
import org.gilbertlang.glibrary.Values._
import org.gilbertlang.glibrary.Types.Helper._
import org.gilbertlang.glibrary.Values.Helper._
import org.gilbertlang.gparser.GParser
import scala.util.parsing.input.StreamReader
import java.io.InputStreamReader
import org.gilbertlang.glibrary.Operators.{ DivOp, PlusOp }

class MTyperTest extends Comparisons {

  @Test def testProgram {
    val ast = ASTProgram(List(ASTAssignment(ASTIdentifier("x"), ASTInteger(12))))
    val expected = TypedProgram(List(TypedAssignment(TypedIdentifier("x", IntegerType), TypedInteger(12))))
    val typer = new GTyper {}
    val result = typer.typeProgram(ast)

    expectResult(expected)(result)
  }

  @Test def testCharacterIntegerUnification {
    val typer = new GTyper {}

    expectResult(Some(IntegerType))(typer.unify(CharacterType, IntegerType))
  }

  @Test def testMatrixMatrixUnification1 {
    val typer = new GTyper {}

    expectResult(Some(MatrixType(IntegerType, IntValue(10), IntValue(42))))(typer.unify(MatrixType(newTV(), newVV(), IntValue(42)),
      MatrixType(IntegerType, IntValue(10), newVV())))
  }

  @Test def testFunctionTyping {
    val typer = new GTyper {}
    val parser = new GParser {}
    val expected = TypedProgram(List(
      TypedFunction(List(TypedIdentifier("X", MatrixType(IntegerType,
        (ValueVar(1)), (ValueVar(2))))),
        TypedIdentifier("foobar", FunctionType(List(
          MatrixType(IntegerType, UniversalValue(ValueVar(1)),
            UniversalValue(ValueVar(2)))), MatrixType(IntegerType,
          UniversalValue(ValueVar(1)), UniversalValue(ValueVar(2))))),
        List(TypedIdentifier("Y", MatrixType(IntegerType, (ValueVar(1)),
          (ValueVar(2))))), TypedProgram(List(
          TypedOutputResultStatement(TypedAssignment(TypedIdentifier("X", MatrixType(IntegerType,
            ValueVar(1), ValueVar(2))),
            TypedBinaryExpression(TypedIdentifier("Y",TypeVar(3)),
              PlusOp, TypedInteger(1), MatrixType(IntegerType, ValueVar(1),
                ValueVar(2))))))))))

    val inputReader = StreamReader(new InputStreamReader(getClass().getClassLoader().getResourceAsStream("function.m")))
    parser.parse(inputReader) match {
      case Some(ast) =>
        val typedAST = typer.typeProgram(ast)
        checkTypeEquality(expected, typedAST)
      case _ => fail("Could not parse input file")
    }

  }

  @Test def testTypeWidening {
    val input = ASTBinaryExpression(ASTInteger(1), DivOp, ASTFloatingPoint(0.1))
    val expected = TypedBinaryExpression(TypedInteger(1), DivOp, TypedFloatingPoint(0.1),DoubleType)
    val typer = new GTyper {}

    val result = typer.typeExpression(input)

    checkTypeEquality(expected,result)
  }

}