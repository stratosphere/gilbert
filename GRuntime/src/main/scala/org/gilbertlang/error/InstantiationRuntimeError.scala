package org.gilbertlang.error

class InstantiationRuntimeError(msg: String) extends RuntimeError(msg) {
	def this() = this("Instation error")
}