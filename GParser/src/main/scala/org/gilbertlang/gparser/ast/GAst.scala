package org.gilbertlang
package gparser
package ast

import glibrary.Operators._

object GAst {
	case class ASTProgram(d: List[ASTStatementOrFunction] )
	
	sealed abstract class ASTStatementOrFunction
	
	case class ASTFunction(values: List[ASTIdentifier], identifier: ASTIdentifier, parameters: List[ASTIdentifier], body: ASTProgram) extends ASTStatementOrFunction
	
	case class ASTTypeAnnotation(annotation: String) extends ASTStatementOrFunction
	
	sealed abstract class ASTStatement  extends ASTStatementOrFunction
	sealed abstract class ASTStatementWithResult extends ASTStatement
	case class ASTOutputResultStatement(statementWithResult: ASTStatementWithResult) extends ASTStatement
	case object ASTNOP extends ASTStatement
	case class ASTAssignment(lhs: ASTIdentifier,rhs:ASTExpression) extends ASTStatementWithResult

	sealed abstract class ASTExpression extends ASTStatementWithResult
	
	case class ASTString(value: String) extends ASTExpression
	case class ASTIdentifier(value: String) extends ASTExpression
	case class ASTMatrix(value: List[ASTMatrixRow]) extends ASTExpression
	case class ASTMatrixRow(value: List[ASTExpression]) extends ASTExpression
	case class ASTUnaryExpression(expression:ASTExpression, operator: UnaryOperator) extends ASTExpression
	case class ASTBinaryExpression(a:ASTExpression, operator: BinaryOperator, b:ASTExpression) extends ASTExpression
	case class ASTFunctionApplication(function:ASTIdentifier, args: List[ASTExpression]) extends ASTExpression
	case class ASTAnonymousFunction(parameters: List[ASTIdentifier], body: ASTExpression) extends ASTExpression
	case class ASTFunctionReference(reference: ASTIdentifier) extends ASTExpression
	
	sealed abstract class ASTScalar extends ASTExpression
	case class ASTInteger(value: Int) extends ASTScalar
	case class ASTFloatingPoint(value: Double) extends ASTScalar	
}
