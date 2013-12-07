package org.gilbertlang.gtyper.errors

class TypingError(msg: String) extends Error(msg) {
	def this() = this("Typing error occured")
}