package org.gilbertlang.mcompiler.errors

class CompileError(msg: String) extends Error(msg) {
  def this() = this("Compile error occurred.")
}