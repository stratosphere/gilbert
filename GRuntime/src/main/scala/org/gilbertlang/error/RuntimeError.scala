package org.gilbertlang.error

class RuntimeError(msg: String) extends Error(msg) {
	def this() = this("Runtime error")
}