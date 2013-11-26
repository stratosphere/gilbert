package de.tuberlin.dima.stratosphere.gilbert.mcompiler.errors

class TypeCompileError(msg: String) extends CompileError {
  def this() = this("Mismatch between type and compiled instructions detected.")

}