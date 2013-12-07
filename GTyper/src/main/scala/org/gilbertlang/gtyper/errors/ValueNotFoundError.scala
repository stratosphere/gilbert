package org.gilbertlang.gtyper.errors

class ValueNotFoundError(msg: String) extends Error(msg) {
  def this() = this("Value not found error")
}