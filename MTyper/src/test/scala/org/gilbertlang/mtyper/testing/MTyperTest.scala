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
import org.gilbertlang.mlibrary.MOperators.{ DivOp, PlusOp }

class MTyperTest extends Comparisons {

  @Test def testProgram {
    val ast = ASTProgram(List(ASTAssignment(ASTIdentifier("x"), ASTInteger(12))))
    val expected = TypedProgram(List(TypedAssignment(TypedIdentifier("x", IntegerType), TypedInteger(12))))
    val typer = new MTyper {}
    val result = typer.typeProgram(ast)

    expectResult(expected)(result)
  }

  @Test def testCharacterIntegerUnification {
    val typer = new MTyper {}

    expectResult(Some(IntegerType))(typer.unify(CharacterType, IntegerType))
  }

  @Test def testMatrixMatrixUnification1 {
    val typer = new MTyper {}

    expectResult(Some(MatrixType(IntegerType, IntValue(10), IntValue(42))))(typer.unify(MatrixType(newTV(), newVV(), IntValue(42)),
      MatrixType(IntegerType, IntValue(10), newVV())))
  }

  @Test def testFunctionTyping {
    val typer = new MTyper {}
    val parser = new MParser {}
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
    val typer = new MTyper {}

    val result = typer.typeExpression(input)

    checkTypeEquality(expected,result)
  }

}