package org.gilbertlang.mlibrary

import MValues.MValue

object MTypes {
  object Helper{
    private var typeVarCounter:Int =0
    def mt(elementType: MType, rows: MValue, columns:MValue) = MatrixType(elementType,rows,columns)
    def pt(types: List[FunctionType]) = PolymorphicType(types)
    def utv = UniversalType(TypeVar())
    def untv = UniversalType(NumericTypeVar())
    
    def newTV() = {
      val result = TypeVar(typeVarCounter)
      typeVarCounter += 1
      result
    }
    
    def newNumericTV() ={
      val result = NumericTypeVar(typeVarCounter)
      typeVarCounter += 1
      result
    }
  }
  
  private val wideableTypes = scala.collection.immutable.Map[MType, List[MType]](
    IntegerType -> List(DoubleType),
    CharacterType -> List(DoubleType, IntegerType))

  sealed trait MType {
    def isWideableTo(other: MType): Boolean =
      this == other || (wideableTypes.getOrElse(this, List()) contains (other))
  }

  trait NumericType extends MType
  case object IntegerType extends NumericType
  case object DoubleType extends NumericType
  case object CharacterType extends MType

  case class FunctionType(parameters: List[MType], value: MType) extends MType {
    def this(parameter: MType, value: MType) = this(List(parameter), value )
  }

  case class MatrixType(elementType: MType, rows: MValue, columns: MValue) extends MType
  
  sealed trait AbstractTypeVar extends MType
  case class NumericTypeVar(id: Int = -1) extends AbstractTypeVar with NumericType
  case class TypeVar(id: Int = -1) extends AbstractTypeVar
  case class UniversalType(universalType: AbstractTypeVar) extends MType
  
  case class PolymorphicType(types:List[MType]) extends MType

  case object VoidType extends MType
  case object UndefinedType extends MType
  
  object FunctionType {
    def apply(parameter: MType, value: MType) = new FunctionType(parameter, value)
    def apply(parameters: (MType, MType), value: MType) = new FunctionType(List(parameters._1, parameters._2), value)
    def apply(parameters: (MType, MType, MType), value: MType) = new FunctionType(List(parameters._1, parameters._2,parameters._3), value)
  }
}