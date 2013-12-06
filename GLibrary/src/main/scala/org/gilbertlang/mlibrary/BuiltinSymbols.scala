package org.gilbertlang.mlibrary

import org.gilbertlang.mlibrary.MTypes.MType

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

  def apply(symbol: String): Option[MType] = {
    getType(symbol)
  }
  
  protected def Symbol(symbol:String, symbolType: MType) = SymbolEntry(symbol,symbolType)
  
  protected case class SymbolEntry(symbol: String, symbolType : MType){
    if(!smap.contains(symbol))
      smap(symbol) = this
  }
}