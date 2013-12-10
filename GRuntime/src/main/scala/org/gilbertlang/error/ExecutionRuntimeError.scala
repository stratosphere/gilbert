package org.gilbertlang.error

class ExecutionRuntimeError(msg: String) extends RuntimeError(msg) {
	def this() = this("Execution runtime error")
}