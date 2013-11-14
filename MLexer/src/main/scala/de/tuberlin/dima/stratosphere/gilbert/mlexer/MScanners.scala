package de.tuberlin.dima.stratosphere.gilbert.mlexer

import scala.util.parsing.combinator.Parsers
import de.tuberlin.dima.stratosphere.gilbert.mlexer.token.MTokens
import scala.util.parsing.input.Reader
import scala.util.parsing.input.CharArrayReader
import de.tuberlin.dima.stratosphere.gilbert.mlexer.token.SelectTokens

trait MScanners extends Parsers with SelectTokens{
	type Token
	type Elem = Char
	
	def token(previousToken: Token): Parser[Token]
	
	def errorToken(msg: String): Token
	
	def voidToken: Token
	
	class MScanner(in: Reader[Elem], previousToken: Token) extends Reader[Token]{
	  def this(in: String) = this(new CharArrayReader(in.toCharArray()), voidToken)
	  
	  def this(in: Reader[Char]) = this(in, voidToken)
	  
	  private val (tok, rest1, rest2) = getNextToken(in)
	  
	  private def getNextToken(in: Reader[Char]): (Token, Reader[Elem], Reader[Elem]) = {
	    token(previousToken)(in) match {
	      case Success(token,in1) => if(accept(token)) (token,in,in1) else getNextToken(in1)
	      case NoSuccess(msg,in1) => (errorToken(msg),in1,skip(in1))
	      }
	  }
	  
	  private def skip(in: Reader[Char]): Reader[Char] = {
	    if (in.atEnd) in else in.rest
	  }
	  
	  override def source= in.source
	  override def offset = in.offset
	  
	  def first = tok
	  def rest = new MScanner(rest2, tok);
	  def pos = rest1.pos;
	  
	  def atEnd = in.atEnd || (getNextToken(in) match { case (_,nextIn,_) => nextIn.atEnd })
	}
}