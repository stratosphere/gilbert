package org.gilbertlang.glibrary

object Values {
  object Helper{
    private var valueVarCounter = 0
    def uv(value: ValueVar) = UniversalValue(value)
    def uvv = UniversalValue(newVV())
    def newVV() = {
      val result = ValueVar(valueVarCounter)
      valueVarCounter += 1
      result
    }
  }
  abstract class Value
  case class ValueVar(id: Int = -1) extends Value
  case class IntValue(value: Int) extends Value
  //case class ExpressionValue(value: TypedExpression) extends MValue
  case class ReferenceValue(reference: Int) extends Value
  case class UniversalValue(value: ValueVar) extends Value{
//    override def equals(uv: Any): Boolean = {
//      (uv,this) match{
//        case (UniversalValue(x:ValueVar), UniversalValue(y:ValueVar)) => true
//        case (UniversalValue(x), UniversalValue(y)) => x == y
//        case _ => false
//      }
//    }
//    
//    override def hashCode():Int = {
//      this match{
//        case UniversalValue(x:ValueVar) => 41*ValueVar().hashCode + ValueVar().hashCode
//        case _ => super.hashCode
//      }
//    }
  }
  case object UndefinedValue extends Value
}