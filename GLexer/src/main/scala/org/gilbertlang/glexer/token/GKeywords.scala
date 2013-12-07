package org.gilbertlang.glexer.token

object GKeywords extends Enumeration{
	type GKeywords = Value
	
	val IF = Value("if");
	val FOR = Value("for");
	val WHILE = Value("while");
	val FUNCTION = Value("function");
	val END = Value("end");
}