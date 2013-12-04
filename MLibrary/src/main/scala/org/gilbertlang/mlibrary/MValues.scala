package org.gilbertlang.mlibrary

object MValues {
  object Helper{
    private var valueVarCounter = 0
    def uv(value: ValueVar) = UniversalValue(value)
    def uvv = UniversalValue(ValueVar())
    def newVV() = {
      val result = ValueVar(valueVarCounter)
      valueVarCounter += 1
      result
    }
  }
  abstract class MValue
  case class ValueVar(id: Int = -1) extends MValue
  case class IntValue(value: Int) extends MValue
  //case class ExpressionValue(value: TypedExpression) extends MValue
  case class ReferenceValue(reference: Int) extends MValue
  case class UniversalValue(value: ValueVar) extends MValue
}