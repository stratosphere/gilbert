package de.tuberlin.dima.stratosphere.gilbert.mlexer.token

object MDelimiters extends Enumeration {
	type MDelimiters = Value
	
	val EQ = Value("=")
	val SEMICOLON = Value(";");
	val COLON = Value(":");
	val COMMA = Value(",");
	val GT = Value(">");
	val LT = Value("<");
	val GTE = Value(">=");
	val LTE = Value("<=");
	val DEQ = Value("==");
	val PLUS = Value("+");
	val MINUS = Value("-");
	val MULT = Value("*");
	val DIV = Value("/");
	val APOSTROPHE = Value("\'");
	val LPAREN = Value("(");
	val RPAREN = Value(")");
	val LSBRACKET = Value("[");
	val RSBRACKET = Value("]");
	val NEWLINE = Value("\n");
}