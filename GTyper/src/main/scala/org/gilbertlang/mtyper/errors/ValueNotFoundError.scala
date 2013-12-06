package org.gilbertlang.mtyper.errors

class ValueNotFoundError(msg: String) extends Error(msg) {
  def this() = this("Value not found error")
}