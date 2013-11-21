package de.tuberlin.dima.stratosphere.gilbert.mtyper.types

import MTypes._
import de.tuberlin.dima.stratosphere.gilbert.mparser.ast.MOperators._
import de.tuberlin.dima.stratosphere.gilbert.mtyper.types.MValues.IntValue

object MTypedAst {
  case class TypedProgram(d: List[TypedStatementOrFunction])

  sealed abstract class TypedStatementOrFunction

  case class TypedFunction(values: List[TypedIdentifier], identifier: TypedIdentifier, parameters: List[TypedExpression], body: TypedProgram) extends TypedStatementOrFunction

  sealed abstract class TypedStatement extends TypedStatementOrFunction
  case class TypedAssignment(lhs: TypedIdentifier, rhs: TypedExpression) extends TypedStatement

  sealed abstract class TypedExpression extends TypedStatement {
    val datatype:MType
  }

  case class TypedString(value: String) extends TypedExpression {
    val datatype = MatrixType(CharacterType, IntValue(1), IntValue(value.length()))
  }
  case class TypedIdentifier(value: String, datatype: MType) extends TypedExpression
  case class TypedMatrix(value: List[TypedMatrixRow], datatype: MatrixType) extends TypedExpression
  case class TypedMatrixRow(value: List[TypedExpression])
  case class TypedUnaryExpression(expression: TypedExpression, operator: UnaryOperator, datatype: MType) extends TypedExpression
  case class TypedBinaryExpression(a: TypedExpression, operator: BinaryOperator, b: TypedExpression, datatype: MType) extends TypedExpression
  case class TypedFunctionApplication(id: TypedIdentifier, args: List[TypedExpression], datatype: MType) extends TypedExpression

  sealed abstract class TypedScalar extends TypedExpression
  case class TypedInteger(value: Int) extends TypedScalar {
    val datatype = IntegerType
  }
  case class TypedFloatingPoint(value: Double) extends TypedScalar {
    val datatype = DoubleType
  }

}