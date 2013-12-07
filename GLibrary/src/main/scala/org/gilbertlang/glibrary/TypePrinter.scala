package org.gilbertlang.mlibrary

import org.gilbertlang.mlibrary.MTypes._

trait TypePrinter extends ValuePrinter{

  def prettyString(mtype: MType): String = {
    mtype match {
      case IntegerType => "Int"
      case DoubleType => "Double"
      case CharacterType => "Char"
      case VoidType => "Void"
      case UndefinedType => "Undefined"
      case PolymorphicType(types) => {
        val concatenatedTypes = types map { prettyString(_) } mkString (", ")
        "(" + concatenatedTypes + ")"
      }
      case FunctionType(args, result) => {
        val concatenatedArgs = args map { prettyString(_) } mkString(", ")
        val resultType = prettyString(result)
        "("+concatenatedArgs + ") => " + resultType
      }
      case MatrixType(elementType, rows, cols) => {
        val elementTypeStr = prettyString(elementType)
        val rowsStr = prettyString(rows)
        val colsStr = prettyString(cols)
        
        "Matrix[" + elementTypeStr + ", " + rowsStr + ", " + colsStr + "]"
      }
      case NumericTypeVar(id) => "ω(" + id + ")"
      case TypeVar(id) => "𝛕("+id+ ")"
      case UniversalType(valueVar) => "∀" + prettyString(valueVar)
    }
  }
}