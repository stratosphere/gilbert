package org.gilbertlang.mlexer.token

import scala.util.parsing.combinator.token._

trait MTokens extends Tokens {
  
  case class Identifier(identifier: String) extends Token{
    def chars = "identifier "+ identifier
  }
  
  case class Keyword(keyword: String) extends Token{
    def chars = "keyword " + keyword
  }
  
  case class StringLiteral(string: String) extends Token{
    def chars = "string " + string
  }
  
  case class IntegerLiteral(value: Int) extends Token{
    def chars = "integer " + value.toString
  }
  
  case class FloatingPointLiteral(value: Double) extends Token{
    def chars = "floating point " + value.toString
  }
  
  case class Comment(value: String) extends Token{
    def chars = "comment " + value
  }
  
  case class TypeAnnotation(value : String) extends Token{
    def chars = "type annotation " + value
  }
  
  case class Whitespace(whitespace: String) extends Token{
    def chars = "whitespace " + whitespace;
  }
  
  case object Void extends Token{
    def chars = "void token"
  }
  
  def voidToken = Void
}