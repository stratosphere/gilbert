package org.gilbertlang.glibrary

import Types.Type

abstract class BuiltinSymbols {
  protected var smap = collection.mutable.HashMap[String,SymbolEntry]()

  def builtinSymbols: Set[String] = {
    smap.keys.toSet
  }
  
  def getType(symbol: String) = {
    smap.get(symbol) match{
      case Some(entry) => Some(entry.symbolType)
      case _ => None
    }
  }
  
  def isSymbol(symbol: String) = {
    smap.contains(symbol)
  }

  def apply(symbol: String): Option[Type] = {
    getType(symbol)
  }
  
  protected def Symbol(symbol:String, symbolType: Type) = SymbolEntry(symbol,symbolType)
  
  protected case class SymbolEntry(symbol: String, symbolType : Type){
    if(!smap.contains(symbol))
      smap(symbol) = this
  }
}