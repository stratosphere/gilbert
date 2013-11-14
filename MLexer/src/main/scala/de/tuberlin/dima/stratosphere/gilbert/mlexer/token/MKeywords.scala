package de.tuberlin.dima.stratosphere.gilbert.mlexer.token

object MKeywords extends Enumeration{
	type MKeywords = Value
	
	val IF = Value("if");
	val FOR = Value("for");
	val WHILE = Value("while");
}