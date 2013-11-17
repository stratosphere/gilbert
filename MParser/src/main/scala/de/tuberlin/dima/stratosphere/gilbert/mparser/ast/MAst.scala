package de.tuberlin.dima.stratosphere.gilbert.mparser.ast

object MAst {
	case class ASTProgram(d: List[ASTStatementOrFunction] )
	
	sealed abstract class ASTStatementOrFunction
	
	case class ASTFunction(values: List[ASTIdentifier], identifier: ASTIdentifier, parameters: List[ASTExpression], body: ASTProgram) extends ASTStatementOrFunction
	
	sealed abstract class ASTStatement  extends ASTStatementOrFunction
	case class ASTAssignment(lhs: ASTIdentifier,rhs:ASTExpression) extends ASTStatement

	sealed abstract class ASTExpression extends ASTStatement
	
	case class ASTString(value: String) extends ASTExpression
	case class ASTIdentifier(value: String) extends ASTExpression
	case class ASTMatrix(value: List[ASTMatrixRow]) extends ASTExpression
	case class ASTMatrixRow(value: List[ASTExpression]) extends ASTExpression
	case class ASTUnaryExpression(expression:ASTExpression, operator: ASTUnaryOperator) extends ASTExpression
	case class ASTBinaryExpression(a:ASTExpression, operator: ASTBinaryOperator, b:ASTExpression) extends ASTExpression
	case class ASTFunctionApplication(id:ASTIdentifier, args: List[ASTExpression]) extends ASTExpression
	
	sealed abstract class ASTScalar extends ASTExpression
	case class ASTInteger(value: Int) extends ASTScalar
	case class ASTFloatingPoint(value: Double) extends ASTScalar
	
	sealed abstract class ASTUnaryOperator
	
	case object ASTTranspose extends ASTUnaryOperator
	case object ASTCellwiseTranspose extends ASTUnaryOperator
	case object ASTPrePlus extends ASTUnaryOperator
	case object ASTPreMinus extends ASTUnaryOperator
	
	sealed abstract class ASTBinaryOperator
	case object ASTExp extends ASTBinaryOperator
	case object ASTCellwiseExp extends ASTBinaryOperator
	case object ASTPlus extends ASTBinaryOperator
	case object ASTMinus extends ASTBinaryOperator
	case object ASTMult extends ASTBinaryOperator
	case object ASTDiv extends ASTBinaryOperator
	case object ASTCellwiseMult extends ASTBinaryOperator
	case object ASTCellwiseDiv extends ASTBinaryOperator
	case object ASTBinaryAnd extends ASTBinaryOperator
	case object ASTBinaryOr extends ASTBinaryOperator
	case object ASTLogicalAnd extends ASTBinaryOperator
	case object ASTLogicalOr extends ASTBinaryOperator
	case object ASTGT extends ASTBinaryOperator
	case object ASTGTE extends ASTBinaryOperator
	case object ASTLT extends ASTBinaryOperator
	case object ASTLTE extends ASTBinaryOperator
	case object ASTDEQ extends ASTBinaryOperator
	
}
