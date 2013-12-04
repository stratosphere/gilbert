package org.gilbertlang.mlibrary

object MOperators {
  sealed abstract class Operator
  
  sealed abstract class UnaryOperator extends Operator
  sealed abstract class CellwiseUnaryOperator extends UnaryOperator

  case object TransposeOp extends UnaryOperator
  case object CellwiseTransposeOp extends CellwiseUnaryOperator
  case object PrePlusOp extends UnaryOperator
  case object PreMinusOp extends UnaryOperator

  sealed abstract class BinaryOperator extends Operator
  sealed abstract class CellwiseBinaryOperator extends BinaryOperator
  
  case object ExpOp extends BinaryOperator
  case object PlusOp extends BinaryOperator
  case object MinusOp extends BinaryOperator
  case object MultOp extends BinaryOperator
  case object DivOp extends BinaryOperator
  
  case object BinaryAndOp extends BinaryOperator
  case object BinaryOrOp extends BinaryOperator
  case object LogicalAndOp extends BinaryOperator
  case object LogicalOrOp extends BinaryOperator
  case object GTOp extends BinaryOperator
  case object GTEOp extends BinaryOperator
  case object LTOp extends BinaryOperator
  case object LTEOp extends BinaryOperator
  case object DEQOp extends BinaryOperator
  
  case object CellwiseMultOp extends CellwiseBinaryOperator
  case object CellwiseDivOp extends CellwiseBinaryOperator
  case object CellwiseExpOp extends CellwiseBinaryOperator
}