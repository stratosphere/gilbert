package org.gilbertlang.gcompiler.errors

class CompileError(msg: String) extends Error(msg) {
  def this() = this("Compile error occurred.")
}