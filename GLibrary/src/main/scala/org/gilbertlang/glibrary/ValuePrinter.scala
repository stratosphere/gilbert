package org.gilbertlang.glibrary

import Values._

trait ValuePrinter {
  
  def prettyString(value: Value): String ={
    value match{
      case UndefinedValue => "Undefined"
      case IntValue(value) => value.toString
      case ReferenceValue(ref) => "$" + ref
      case UniversalValue(valueVar) => "∀" + prettyString(valueVar)
      case ValueVar(id) => "φ(" + id +")"
    }
  }

}