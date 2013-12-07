package org.gilbertlang.mtyper.errors

class NotYetImplementedError(msg:String) extends Error(msg) {
  def this() = this("Functionality not yet implemented")

}