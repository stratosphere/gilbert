package org.gilbertlang.mlexer.token

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
	val CELLWISE_MULT = Value(".*");
	val DIV = Value("/");
	val CELLWISE_DIV = Value("./");
	val TRANSPOSE = Value("\'");
	val CELLWISE_TRANSPOSE = Value(".\'")
	val DQUOTE = Value("\"");
	val LPAREN = Value("(");
	val RPAREN = Value(")");
	val LBRACKET = Value("[");
	val RBRACKET = Value("]");
	val LBRACE = Value("{");
	val RBRACE = Value("}");
	val NEWLINE = Value("\n");
	val LOGICAL_OR = Value("||");
	val LOGICAL_AND = Value("&&");
	val BINARY_OR = Value("|");
	val BINARY_AND = Value("&");
	val EXP = Value("^");
	val CELLWISE_EXP = Value(".^")
	val AT = Value("@")
}