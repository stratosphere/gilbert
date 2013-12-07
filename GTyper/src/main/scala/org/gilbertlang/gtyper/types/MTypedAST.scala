package org.gilbertlang
package gtyper.types

import glibrary.Types._
import glibrary.Operators._
import glibrary.Values.IntValue
import glibrary.Types

object GTypedAst {
   def getType(stmt : TypedStatement): Type = {
    stmt match{
      case x:TypedExpression => x.datatype
      case TypedOutputResultStatement(innerStmt) => getType(innerStmt)
      case TypedAssignment(lhs,rhs) => rhs.datatype
      case TypedNOP => VoidType
    }
  }
   
  case class TypedProgram(d: List[TypedStatementOrFunction])

  sealed abstract class TypedStatementOrFunction

  case class TypedFunction(values: List[TypedIdentifier], identifier: TypedIdentifier, 
      parameters: List[TypedIdentifier], body: TypedProgram) extends TypedStatementOrFunction
  sealed abstract class TypedStatement extends TypedStatementOrFunction
  
  sealed abstract class TypedStatementWithResult extends TypedStatement
  case object TypedNOP extends TypedStatement
  case class TypedOutputResultStatement(statementWithResult: TypedStatementWithResult) extends TypedStatement
  case class TypedAssignment(lhs: TypedIdentifier, rhs: TypedExpression) extends TypedStatementWithResult

  sealed abstract class TypedExpression extends TypedStatementWithResult {
    val datatype:Type
  }

  case class TypedString(value: String) extends TypedExpression {
    val datatype = MatrixType(CharacterType, IntValue(1), IntValue(value.length()))
  }
  case class TypedIdentifier(value: String, datatype: Type) extends TypedExpression
  case class TypedMatrix(value: List[TypedMatrixRow], datatype: MatrixType) extends TypedExpression
  case class TypedMatrixRow(value: List[TypedExpression])
  case class TypedUnaryExpression(expression: TypedExpression, operator: UnaryOperator, datatype: Type) extends TypedExpression
  case class TypedBinaryExpression(a: TypedExpression, operator: BinaryOperator, b: TypedExpression, datatype: Type) extends TypedExpression
  case class TypedFunctionApplication(id: TypedIdentifier, args: List[TypedExpression], datatype: Type) extends TypedExpression
  case class TypedAnonymousFunction(parameters: List[TypedIdentifier], body: TypedExpression, datatype: Type) extends TypedExpression
  case class TypedFunctionReference(reference: TypedIdentifier, datatype: Type) extends TypedExpression

  sealed abstract class TypedScalar extends TypedExpression
  case class TypedInteger(value: Int) extends TypedScalar {
    val datatype = IntegerType
  }
  case class TypedFloatingPoint(value: Double) extends TypedScalar {
    val datatype = DoubleType
  }

}