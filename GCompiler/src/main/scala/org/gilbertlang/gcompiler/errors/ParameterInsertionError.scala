package org.gilbertlang.gcompiler.errors

class ParameterInsertionError(msg: String) extends Error(msg) {
	def this() = this("Parameter insertion error")
}