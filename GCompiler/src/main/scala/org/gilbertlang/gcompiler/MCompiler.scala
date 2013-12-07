package org.gilbertlang.gcompiler

import org.gilbertlang._
import org.gilbertlang.gtyper.types.GTypedAst._
import org.gilbertlang.glibrary.ConvenienceMethods
import org.gilbertlang.glibrary.Types.MatrixType
import org.gilbertlang.glibrary.Types.NumericType
import org.gilbertlang.glibrary.Operators._
import errors.TypeCompileError
import org.gilbertlang.glibrary.GBuiltinSymbols
import org.gilbertlang.gtyper.types.GTypedAst

trait MCompiler {
  val assignments = scala.collection.mutable.Map[String, Executable]()
  val functions = scala.collection.mutable.Map[String, Executable]()

  private def registerFunction(functionName: String, executable: Executable) {
    functions.update(functionName, executable)
  }

  private def assign(identifier: String, executable: Executable) {
    assignments.update(identifier, executable)
  }

  private def retrieveExecutable(id: String): Executable = {
    assignments.getOrElse(id, EmptyExecutable)
  }

  def compile(typedProgram: TypedProgram): Executable = {
    typedProgram match {
      case TypedProgram(stmtsOrFunctions) =>
        val functions = stmtsOrFunctions collect { case x: TypedFunction => x }
        val stmts = stmtsOrFunctions collect { case x: TypedStatement => x }
        functions.map { compileFunction(_) }.foreach({ (registerFunction _).tupled(_) })
        CompoundExecutable(stmts flatMap { compileStatementWithResult(_) })
    }
  }

  def compileExpression(typedExpression: TypedExpression): Executable = {
    typedExpression match {
      case x: TypedIdentifier => compileIdentifier(x)
      case x: TypedInteger => scalar(x.value)
      case x: TypedFloatingPoint => scalar(x.value)
      case x: TypedString => string(x.value)
      case x: TypedUnaryExpression => compileUnaryExpression(x)
      case x: TypedBinaryExpression => compileBinaryExpression(x)
      case x: TypedFunctionApplication => compileFunctionApplication(x)
    }
  }

  def compileIdentifier(identifier: TypedIdentifier) = {
    identifier match {
      case TypedIdentifier(id, _) =>
        if (MBuiltinSymbols.isSymbol(id)) {
          compileBuiltInSymbol(id)
        } else {
          retrieveExecutable(id)
        }
    }
  }

  def compileBuiltInSymbol(symbol: String) = {
    EmptyExecutable
  }

  def compileUnaryExpression(unaryExpression: TypedUnaryExpression) = {
    val exec = compileExpression(unaryExpression.expression)
    unaryExpression.operator match {
      case PrePlusOp => exec
      case PreMinusOp => exec match {
        case x: Matrix => CellwiseMatrixTransformation(x, ScalarOperation.Negate)
        case x: ScalarRef => ScalarTransformation(x, UnaryScalarOperation.UnaryMinus)
      }
      case TransposeOp | CellwiseTransposeOp => exec match {
        case x: Matrix => Transpose(x)
        case x: ScalarRef => x
      }
    }
  }

  def compileBinaryExpression(binaryExpression: TypedBinaryExpression) = {
    val a = compileExpression(binaryExpression.a)
    val b = compileExpression(binaryExpression.b)

    binaryExpression.operator match {
      case ExpOp => (a, b) match {
        case (x: Matrix, y: ScalarRef) => MatrixExponentiation(x, y)
        case (x: ScalarRef, y: ScalarRef) => ScalarScalarTransformation(x, y, ScalarsOperation.Exponentiation)
      }
      case MultOp => (a, b) match {
        case (x: Matrix, y: Matrix) => MatrixMult(x, y)
        case (x: Matrix, y: ScalarRef) => ScalarMatrixTransformation(y, x, ScalarsOperation.Multiplication)
        case (x: ScalarRef, y: Matrix) => ScalarMatrixTransformation(x, y, ScalarsOperation.Multiplication)
        case (x: ScalarRef, y: ScalarRef) => ScalarScalarTransformation(x, y, ScalarsOperation.Multiplication)
      }
      case CellwiseMultOp => (a, b) match {
        case (x: Matrix, y: Matrix) => CellwiseMatrixMatrixTransformation(x, y, CellwiseOperation.Multiplication)
        case (x: Matrix, y: ScalarRef) => ScalarMatrixTransformation(y, x, ScalarsOperation.Multiplication)
        case (x: ScalarRef, y: Matrix) => ScalarMatrixTransformation(x, y, ScalarsOperation.Multiplication)
        case (x: ScalarRef, y: ScalarRef) => ScalarScalarTransformation(x, y, ScalarsOperation.Multiplication)
      }
      case DivOp => (a, b) match {
        case (x: ScalarRef, y: ScalarRef) => ScalarScalarTransformation(x, y, ScalarsOperation.Division)
        case (x: Matrix, y: ScalarRef) => ScalarMatrixTransformation(y, x, ScalarsOperation.Division)
      }
      case CellwiseDivOp => (a, b) match {
        case (x: Matrix, y: Matrix) => CellwiseMatrixMatrixTransformation(x, y, CellwiseOperation.Division)
        case (x: Matrix, y: ScalarRef) => ScalarMatrixTransformation(y, x, ScalarsOperation.Division)
        case (x: ScalarRef, y: ScalarRef) => ScalarScalarTransformation(x, y, ScalarsOperation.Division)
      }
    }
  }

  def compileFunctionApplication(functionApplication: TypedFunctionApplication) = {
    functionApplication match {
      case TypedFunctionApplication(TypedIdentifier(id, _), parameters, _) =>
        val execs = parameters map { compileExpression(_) }
        id match {
          case "load" => execs match {
            case List(string(file), scalar(rows), scalar(cols)) => LoadMatrix(file, rows.toInt, cols.toInt)
          }
          case "binarize" => execs match {
            case List(x: Matrix) => CellwiseMatrixTransformation(x, ScalarOperation.Binarize)
          }
          case "maxValue" => execs match {
            case List(x: Matrix) => AggregateMatrixTransformation(x, ScalarsOperation.Maximum)
          }
        }
    }
  }

  def compileFunction(typedFunction: TypedFunction): (String, Executable) = {
    (typedFunction.identifier.value, EmptyExecutable)
  }

  def compileStatementWithResult(typedStatement: TypedStatement): Option[Executable] = {
    typedStatement match {
      case TypedAssignment(lhs, rhs) =>
        val result = compileExpression(rhs)
        assign(lhs.value, result)
        None
      case x: TypedExpression =>
        compileExpression(x)
        None
      case TypedNOP => None
      case TypedOutputResultStatement(stmt) =>
        MTypedAst.getType(stmt) match {
          case _: MatrixType =>
            compileStatement(stmt) match {
              case x: Matrix => Some(WriteMatrix(x))
              case _ => throw new TypeCompileError("Expected executable of type Matrix")
            }
          case _: NumericType =>
            compileStatement(stmt) match {
              case x: ScalarRef => Some(WriteScalarRef(x))
              case _ => throw new TypeCompileError("Expected executable of type ScalarRef")
            }
          case _ => None
        }
    }
  }

  def compileStatement(typedStatement: TypedStatement): Executable = {
    typedStatement match {
      case TypedAssignment(lhs, rhs) =>
        val result = compileExpression(rhs)
        assign(lhs.value, result)
        result
      case x: TypedExpression => compileExpression(x)
      case TypedNOP => EmptyExecutable
      case TypedOutputResultStatement(stmt) =>
        MTypedAst.getType(stmt) match {
          case _: MatrixType =>
            compileStatement(stmt) match {
              case x: Matrix => WriteMatrix(x)
              case _ => throw new TypeCompileError("Expected executable of type Matrix")
            }
          case _: NumericType =>
            compileStatement(stmt) match {
              case x: ScalarRef => WriteScalarRef(x)
              case _ => throw new TypeCompileError("Expected executable of type ScalarRef")
            }
          case _ => EmptyExecutable
        }
    }
  }
}