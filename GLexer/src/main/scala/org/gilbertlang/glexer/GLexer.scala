package org.gilbertlang.glexer

import scala.util.parsing.input.Reader
import token.GTokens
import scala.collection.mutable.ListBuffer
import scala.util.parsing.input.CharArrayReader
import scala.util.parsing.input.CharArrayReader.EofCh
import token.GKeywords
import scala.collection.immutable.HashSet
import token.GDelimiters

trait GLexer extends GScanners with GTokens {
  
  val keywords = HashSet[String]((for(value <- GKeywords.values) yield value.toString).toSeq:_*)
  
  def accept(token: Token) = { true }
  
  def letter:Parser[Char] = elem("letter",_.isLetter);
  
  def letter(chr: Char): Parser[Char] = elem(chr.toString,_ == chr)
  
  def digit:Parser[Char] = elem("digit", _.isDigit);
  
  def whitespace:Parser[Char] = elem("whitespace", ch => ch <= ' ' && ch != EofCh && ch != '\n');
  
  def chrExcept(except: List[Char]):Parser[Char] = elem("Except: "+except.mkString(""),chr => except forall ( _ != chr))
  
  implicit def tilde2Str[A <: {def mkString(delim:String):String},B <:{def mkString(delim:String):String}](x: ~[A,B]):{def mkString(delim:String):String} = { new { def mkString(delim:String):String = x._1.mkString("") + x._2.mkString("")}}

  def token(previousToken: Token): Parser[Token] =  ( 
      letter ~ rep(letter | digit | '_') ^^ { case h~t => processIdentifier(h+(t.mkString("")))}
      | digit ~ rep(digit) ~ opt('.' ~ rep(digit)) ~ (letter('e') | letter('E')) ~ opt(letter('+') | letter('-')) ~ digit ~ rep(digit) ^^ {
        case h~t~p~l~s~e~r => 
          val a = (h::t).mkString("")
          val b = p match {
            case Some(p~l) => p+l.mkString("")
            case None => ""
          }
          val c = s match{
            case Some(pm) => pm
            case None => ""
          }
          
          val d = (e::r).mkString("");
          
          FloatingPointLiteral((a+b+l+c+d).toDouble)
      }
      | digit ~ rep(digit) ~ '.' ~ rep(digit) ^^ { case h~t~p~r => FloatingPointLiteral((h+t.mkString("")+p+r.mkString("")).toDouble) }
      | '.' ~ digit ~ rep(digit) ^^ { case p~h~r => FloatingPointLiteral(("0"+p+h+r.mkString("")).toDouble) }
      | digit ~ rep(digit) ^^ { case h~t => IntegerLiteral((h::t).dropWhile(_=='0').mkString("").toInt) }
      | whitespace ~ rep(whitespace) ^^ { case h~t => Whitespace(h+t.mkString(""))}
      | guard(Parser{in => if(isTransposable(previousToken)) Failure("failure",in) else Success("success",in)})~>'\''~>rep( chrExcept(List('\'','\n',EofCh)))<~'\'' ^^ { case l => StringLiteral(l.mkString("")) }
      | '\"'~>rep( chrExcept(List('\"','\n',EofCh)))<~'\"' ^^ { case l => StringLiteral(l.mkString("")) }
      | '%' ~> '>' ~> rep(chrExcept(List('\n',EofCh))) ^^ { case l => TypeAnnotation(l.mkString("")) }
      | '%'~> rep(chrExcept(List('\n', EofCh))) ^^ {case l => Comment(l.mkString(""))}
      | EofCh ^^^ EOF
      | delimiterParser
      | failure("illegal character"))
      
  private def isTransposable(token:Token):Boolean = {
    token match {
      case Identifier(_) | Keyword(")") | Keyword("]") => true
      case _ => false
    }
  }
  
  private def processIdentifier(identifier: String):Token = {
    if(keywords contains identifier)
      Keyword(identifier)
    else
      Identifier(identifier)
  }
  
  private lazy val delimiters = {
    val delimiterValues = for(value <- GDelimiters.values) yield value.toString
    val sortedDelimiterValues = delimiterValues.toList.sortWith(_.length >= _.length);
    
    (sortedDelimiterValues map ( s => accept(s.toList) ^^^ Keyword(s) )).
    foldRight(failure("No such delimiter found"):Parser[Token])((delimiter,right) => delimiter | right);
  }
  
  private def delimiterParser:Parser[Token] = delimiters
}