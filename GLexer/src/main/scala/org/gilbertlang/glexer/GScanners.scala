package org.gilbertlang.glexer

import scala.util.parsing.combinator.Parsers
import token.GTokens
import scala.util.parsing.input.Reader
import scala.util.parsing.input.CharArrayReader
import token.SelectTokens
import scala.collection.mutable.ListBuffer
import org.gilbertlang.input.EOFReader

trait GScanners extends Parsers with SelectTokens {
  type Token
  type Elem = Char

  def token(previousToken: Token): Parser[Token]

  def errorToken(msg: String): Token

  def voidToken: Token

  def lex(in: String): List[Token] = lex(new CharArrayReader(in.toCharArray()));

  def lex(in: Reader[Char]): List[Token] = {
    var scanner = GScanners.this(in)
    val listBuffer = new ListBuffer[Token]();

    while (!scanner.atEnd) {
      listBuffer += scanner.first;
      scanner = scanner.rest;
    }

    listBuffer.toList
  }

  def apply(in: Reader[Char]) = new GScanner(in)
  def apply(in: String) = new GScanner(in)
  
  implicit def reader2EOFReader[T](reader: Reader[T]):EOFReader[T] = {
    reader match{
      case x:EOFReader[T] => x
      case _ => EOFReader(reader)
    }
  }

  class GScanner(in: EOFReader[Elem], previousToken: Token) extends Reader[Token] {
    def this(in: String) = this(EOFReader(new CharArrayReader(in.toCharArray())), voidToken)

    def this(in: EOFReader[Char]) = this(in, voidToken)

    private val (tok, rest1, rest2) = getNextToken(in)

    private def getNextToken(in: EOFReader[Char]): (Token, EOFReader[Elem], EOFReader[Elem]) = {
      token(previousToken)(in) match {
        case Success(token, in1:EOFReader[Elem]) => if (accept(token)) (token, in, in1) else getNextToken(in1)
        case NoSuccess(msg, in1:EOFReader[Elem]) => (errorToken(msg), in1, skip(in1))
      }
    }

    private def skip(in: EOFReader[Char]): EOFReader[Char] = {
      if (in.atEnd) in else in.rest
    }

    override def source = in.source
    override def offset = in.offset

    def first = tok
    def rest = new GScanner(rest2, tok);
    def pos = rest1.pos;

    def atEnd = in.atEnd || (getNextToken(in) match { case (_, nextIn, _) => nextIn.atEnd })
  }
}