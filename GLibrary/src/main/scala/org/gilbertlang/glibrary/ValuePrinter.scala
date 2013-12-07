package org.gilbertlang.mlibrary

import org.gilbertlang.mlibrary.MValues._

trait ValuePrinter {
  
  def prettyString(value: MValue): String ={
    value match{
      case UndefinedValue => "Undefined"
      case IntValue(value) => value.toString
      case ReferenceValue(ref) => "$" + ref
      case UniversalValue(valueVar) => "∀" + prettyString(valueVar)
      case ValueVar(id) => "φ(" + id +")"
    }
  }

}