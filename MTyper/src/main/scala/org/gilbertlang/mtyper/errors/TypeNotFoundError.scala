package de.tuberlin.dima.stratosphere.gilbert.mtyper.errors

class TypeNotFoundError(msg: String) extends TypingError(msg) {
  def this() = this("Type was not found exception.")
}