package org.gilbertlang.mparser

import scala.util.parsing.combinator.Parsers
import org.gilbertlang.mlexer.token.MTokens
import org.gilbertlang.mlexer.MScanners
import org.gilbertlang.mlexer.MLexer
import scala.util.parsing.input.Reader
import org.gilbertlang.mlexer.token.MKeywords
import org.gilbertlang.mlexer.token.MDelimiters
import org.gilbertlang.mlexer.token.DiscardWhitespaces
import org.gilbertlang.mparser.ast.MAst
import org.gilbertlang.mlibrary.MOperators._
import org.gilbertlang.mlexer.token.DiscardComments

trait MParser extends Parsers {
  import MAst._
  import language.implicitConversions

  val lexer = new MLexer with DiscardWhitespaces with DiscardComments {}
  type Elem = lexer.Token

  import lexer.{ Keyword, Identifier, StringLiteral, IntegerLiteral, FloatingPointLiteral, EOF, TypeAnnotation }

  import MKeywords._
  import MDelimiters._

  implicit def tokenReader(input: Reader[Char]): Reader[Elem] = lexer(input)
  implicit def tokenReader(input: String): Reader[Elem] = lexer(input)
  implicit def keyword2Parser(keyword: MKeywords): Parser[MKeywords] = this.elem("keyword " + keyword.toString, 
      { case Keyword(name) if name == keyword.toString => true case _ => false }) ^^ { _ => keyword } 
  implicit def delimiter2Parser(delimiter: MDelimiters): Parser[MDelimiters] = this.elem("keyword " + 
      delimiter.toString, { case Keyword(name) if name == delimiter.toString => true case _ => false }) ^^ 
      { _ => delimiter }

  def program = statementOrFunctionList

  def statementOrFunctionList: Parser[ASTProgram] = rep(newlineOrCommaOrSemicolon) ~> rep((statement | functionDefinition | typeAnnotation) <~ 
      rep(newlineOrCommaOrSemicolon) <~ opt(EOF)) ^^ { case l => ASTProgram(l) }

  def functionDefinition = (FUNCTION ~> opt(functionValues)) ~ identifier ~ functionParams ~ newlineOrCommaOrSemicolon ~
  functionBody <~ END <~ (newlineOrCommaOrSemicolon | eof) ^^ {
    case None ~ id ~ funParams ~ _ ~ funBody => ASTFunction(Nil, id, funParams, funBody)
    case Some(funValues) ~ id ~ funParams ~ _ ~ funBody => ASTFunction(funValues, id, funParams, funBody)
  }

  def functionValues = (identifier <~ EQ ^^ { i => List(i) }
    | (LBRACKET ~> identifierList) <~ RBRACKET <~ EQ
    | failure("function value expected"))

  def functionParams = LPAREN ~> identifierList <~ RPAREN

  def functionBody = statementOrFunctionList 
  
  def identifierList = repsep(identifier,COMMA)

  def identifier = elem("identifier", { case Identifier(_) => true case _ => false }) ^^ 
  { case Identifier(id) => ASTIdentifier(id) }
  
  def typeAnnotation = elem("type annotation", { case TypeAnnotation(_) => true case _ => false}) ^^
  { case TypeAnnotation(value) => ASTTypeAnnotation(value)}

  def identifierWithIndex: Parser[ASTIdentifier] = failure("identifier with index is not yet supported")
  
  def statement = statementWithResult | statementWithoutResult
  
  def nop = (newlineOrCommaOrSemicolon | eof) ^^ { x => ASTNOP}
  
  def statementWithoutResult = (forStatement | whileStatement) <~ (newlineOrCommaOrSemicolon | eof)

  def statementWithResult = (assignment | expression ) ~ (newlineOrCommaOrSemicolon | eof) ^^ { 
    case stmt~SEMICOLON => stmt
    case stmt~_ => ASTOutputResultStatement(stmt) } 

  def forStatement: Parser[ASTStatement] = failure("for statement is not yet supported")

  def whileStatement: Parser[ASTStatement] = failure("while statement is not yet supported")

  def assignment = ((lhs <~ EQ) ~ rhs) ^^ { case l ~ r => ASTAssignment(l, r) }

  def lhs = (identifier
    | identifierWithIndex)

  def rhs = expression

  def expression: Parser[ASTExpression] = arithmeticExpression

  def arithmeticExpression = aexp1

  def aexp1: Parser[ASTExpression] = aexp2 ~ rep(LOGICAL_OR ~> aexp2) ^^ {
    case e ~ Nil => e
    case e ~ l => l.foldLeft(e) { (x, y) => ASTBinaryExpression(x, LogicalOrOp, y)}
  }

  def aexp2 = aexp3 ~ rep(LOGICAL_AND ~> aexp3) ^^ {
    case e ~ Nil => e
    case e ~ l => l.foldLeft(e) { (x, y) => ASTBinaryExpression(x, LogicalAndOp, y) }
  }

  def aexp3 = aexp4 ~ rep(BINARY_OR ~> aexp4) ^^ {
    case e ~ Nil => e
    case e ~ l => l.foldLeft(e) { (x, y) => ASTBinaryExpression(x, BinaryOrOp, y) }
  }

  def aexp4 = aexp5 ~ rep(BINARY_AND ~> aexp5) ^^ {
    case e ~ Nil => e
    case e ~ l => l.foldLeft(e) { (x, y) => ASTBinaryExpression(x, BinaryAndOp, y) }
  }

  def aexp5 = aexp6 ~ rep(comparisonOperator ~ aexp6) ^^ {
    case e ~ Nil => e
    case e ~ l => l.foldLeft(e)((x, y) => y match {
      case GT ~ c => ASTBinaryExpression(x, GTOp, c)
      case GTE ~ c => ASTBinaryExpression(x, GTEOp, c)
      case LT ~ c => ASTBinaryExpression(x, LTOp, c)
      case LTE ~ c => ASTBinaryExpression(x, LTEOp, c)
      case DEQ ~ c => ASTBinaryExpression(x, DEQOp, c)
    })
  }

  def aexp6 = aexp7 ~ repsep(aexp7, COLON) ^^ { case e ~ _ => e }

  def aexp7 = aexp8 ~ rep(additionOperator ~ aexp8) ^^ {
    case e ~ Nil => e
    case e ~ l => l.foldLeft(e)((x, y) => y match {
      case PLUS ~ a => ASTBinaryExpression(x, PlusOp, a)
      case MINUS ~ a => ASTBinaryExpression(x, MinusOp, a)
    })
  }

  def aexp8 = aexp9 ~ rep(multiplicationOperator ~ aexp9) ^^ {
    case e ~ Nil => e
    case e ~ l => l.foldLeft(e)((x, y) => y match {
      case MULT ~ m => ASTBinaryExpression(x, MultOp, m)
      case DIV ~ m => ASTBinaryExpression(x, DivOp, m)
      case CELLWISE_DIV ~ m => ASTBinaryExpression(x, CellwiseDivOp, m)
      case CELLWISE_MULT ~ m => ASTBinaryExpression(x, CellwiseMultOp, m)
    })
  }

  def aexp9: Parser[ASTExpression] = (prefix_operator ~ aexp9 ^^ {
    case PLUS ~ e => ASTUnaryExpression(e, PrePlusOp)
    case MINUS ~ e => ASTUnaryExpression(e, PreMinusOp)
  }
    | aexp10)

  def aexp10 = aexp11 ~ rep(exponentiationOperator ~ aexp11) ^^ {
    case e ~ Nil => e
    case e ~ l => l.foldLeft(e)((x, y) => y match {
      case EXP ~ exp => ASTBinaryExpression(x, ExpOp, exp)
      case CELLWISE_EXP ~ exp => ASTBinaryExpression(x, CellwiseExpOp, exp)
    })
  }

  def aexp11 = unaryExpression ~ opt(postFixOperator) ^^ {
    case e ~ Some(TRANSPOSE) => ASTUnaryExpression(e, TransposeOp)
    case e ~ Some(CELLWISE_TRANSPOSE) => ASTUnaryExpression(e, CellwiseTransposeOp)
    case e ~ None => e
  }

  def unaryExpression = elementaryExpression | LPAREN ~> expression <~ RPAREN

  def elementaryExpression: Parser[ASTExpression] = (functionApplication
    | identifier
    | scalar
    | matrix
    | stringLiteral
    | anonymousFunction
    | functionReference)
    
    def functionReference = AT ~> identifier ^^ { x => ASTFunctionReference(x)}
    
    def anonymousFunction = (AT ~> LPAREN ~> identifierList <~ RPAREN) ~ expression ^^ 
    { case parameters ~ expression => ASTAnonymousFunction(parameters, expression)}
    
  def functionApplication = identifier ~ LPAREN ~ repsep(expression, COMMA) ~ RPAREN ^^ 
  { case exp~LPAREN~args~RPAREN => ASTFunctionApplication(exp,args) }

  def scalar: Parser[ASTScalar] = integerLiteral | floatingPointLiteral

  def integerLiteral = elem("integer", { case IntegerLiteral(_) => true case _ => false }) ^^ 
  { case IntegerLiteral(i) => ASTInteger(i) }
  def floatingPointLiteral = elem("floating point", { case FloatingPointLiteral(_) => true case _ => false }) ^^ 
  { case FloatingPointLiteral(f) => ASTFloatingPoint(f) }
  def stringLiteral = elem("string", { case StringLiteral(_) => true case _ => false }) ^^ 
  { case StringLiteral(s) => ASTString(s) }

  def matrix = LBRACKET ~> repsep(matrixRow, newlineOrSemicolon) <~ RBRACKET ^^ { m => ASTMatrix(m) }
  def matrixRow = repsep(expression, COMMA) ^^ { r => ASTMatrixRow(r) }

  def prefix_operator = ( PLUS 
    | MINUS  )
  def multiplicationOperator = (MULT 
    | DIV 
    | CELLWISE_MULT
    | CELLWISE_DIV )
  def additionOperator = PLUS  | MINUS
  def comparisonOperator = GT | GTE | LT | LTE | DEQ
  def postFixOperator = CELLWISE_TRANSPOSE | TRANSPOSE
  def exponentiationOperator = EXP | CELLWISE_EXP

  def newlineOrSemicolon = SEMICOLON | NEWLINE
  def newlineOrCommaOrSemicolon = SEMICOLON | NEWLINE | COMMA
  def eof = accept(EOF)
  
  def parse(input: Reader[Char]) = {
    phrase(program)(input) match{
      case Success(result,_) => Some(result)
      case _ => None
    }
  }
}
