package org.gilbertlang.glibrary

import Values.Value

object Types {
  object Helper{
    private var typeVarCounter:Int =0
    def mt(elementType: Type, rows: Value, columns:Value) = MatrixType(elementType,rows,columns)
    def pt(types: List[FunctionType]) = PolymorphicType(types)
    def utv = UniversalType(newTV())
    def untv = UniversalType(newNumericTV())
    
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
  
  private val wideableTypes = scala.collection.immutable.Map[Type, List[Type]](
    IntegerType -> List(DoubleType),
    CharacterType -> List(DoubleType, IntegerType))

  sealed trait Type {
    def isWideableTo(other: Type): Boolean =
      Type.this == other || (wideableTypes.getOrElse(Type.this, List()) contains (other))
  }

  case object StringType extends Type
  sealed trait NumericType extends Type
  case object IntegerType extends NumericType
  case object DoubleType extends NumericType
  case object CharacterType extends Type

  case class FunctionType(parameters: List[Type], value: Type) extends Type {
    def this(parameter: Type, value: Type) = this(List(parameter), value )
  }

  case class MatrixType(elementType: Type, rows: Value, columns: Value) extends Type
  
  sealed trait AbstractTypeVar extends Type
  case class NumericTypeVar(id: Int = -1) extends AbstractTypeVar with NumericType
  case class TypeVar(id: Int = -1) extends AbstractTypeVar
  case class UniversalType(universalType: AbstractTypeVar) extends Type
  
  case class PolymorphicType(types:List[Type]) extends Type

  case object VoidType extends Type
  case object UndefinedType extends Type
  
  object FunctionType {
    def apply(parameter: Type, value: Type) = new FunctionType(parameter, value)
    def apply(parameters: (Type, Type), value: Type) = new FunctionType(List(parameters._1, parameters._2), value)
    def apply(parameters: (Type, Type, Type), value: Type) = new FunctionType(List(parameters._1, parameters._2,parameters._3), value)
  }
}