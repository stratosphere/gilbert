package org.gilbertlang.gtyper.errors

class NotYetImplementedError(msg:String) extends Error(msg) {
  def this() = this("Functionality not yet implemented")

}