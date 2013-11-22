package de.tuberlin.dima.stratosphere.gilbert.mparser

import scala.util.parsing.combinator.Parsers
import de.tuberlin.dima.stratosphere.gilbert.mlexer.token.MTokens
import de.tuberlin.dima.stratosphere.gilbert.mlexer.MScanners
import de.tuberlin.dima.stratosphere.gilbert.mlexer.MLexer
import scala.util.parsing.input.Reader
import de.tuberlin.dima.stratosphere.gilbert.mlexer.token.MKeywords
import de.tuberlin.dima.stratosphere.gilbert.mlexer.token.MDelimiters
import de.tuberlin.dima.stratosphere.gilbert.mlexer.token.DiscardWhitespaces
import de.tuberlin.dima.stratosphere.gilbert.mparser.ast.MAst
import de.tuberlin.dima.stratosphere.gilbert.mparser.ast.MOperators._

trait MParser extends Parsers {
  import MAst._
  import language.implicitConversions

  val lexer = new MLexer with DiscardWhitespaces {}
  type Elem = lexer.Token

  import lexer.{ Keyword, Identifier, StringLiteral, IntegerLiteral, FloatingPointLiteral, EOF }

  import MKeywords._
  import MDelimiters._

  implicit def tokenReader(input: Reader[Char]): Reader[Elem] = lexer(input)
  implicit def tokenReader(input: String): Reader[Elem] = lexer(input)
  implicit def keyword2Parser(keyword: MKeywords): Parser[MKeywords] = this.elem("keyword " + keyword.toString, { case Keyword(name) if name == keyword.toString => true case _ => false }) ^^ { _ => keyword } 
  implicit def delimiter2Parser(delimiter: MDelimiters): Parser[MDelimiters] = this.elem("keyword " + delimiter.toString, { case Keyword(name) if name == delimiter.toString => true case _ => false }) ^^ { _ => delimiter }

  def program = statementOrFunctionList

  def statementOrFunctionList: Parser[ASTProgram] = rep((statement | functionDefinition) <~ (newlineOrCommaOrSemicolon | eof)) ^^ { case l => ASTProgram(l) }

  def functionDefinition = (FUNCTION ~> opt(functionValues)) ~ identifier ~ functionParams ~ functionBody <~ END ^^ {
    case None ~ id ~ funParams ~ funBody => ASTFunction(Nil, id, funParams, funBody)
    case Some(funValues) ~ id ~ funParams ~ funBody => ASTFunction(funValues, id, funParams, funBody)
  }

  def functionValues = (identifier <~ EQ ^^ { i => List(i) }
    | (LBRACKET ~> identifierList) <~ RBRACKET <~ EQ
    | failure("function value expected"))

  def functionParams = (LPAREN ~> identifierList) <~ RPAREN

  def functionBody = (LBRACE ~> statementOrFunctionList) <~ RBRACE

  def identifierList = identifier ~ repsep(identifier, COMMA) ^^ { case h ~ t => h :: t }

  def identifier = elem("identifier", { case Identifier(_) => true case _ => false }) ^^ { case Identifier(id) => ASTIdentifier(id) }

  def identifierWithIndex: Parser[ASTIdentifier] = failure("identifier with index is not yet supported")

  def statement = (assignment
    | expression
    | forStatement
    | whileStatement)

  def forStatement: Parser[ASTStatement] = failure("for statement is not yet supported")

  def whileStatement: Parser[ASTStatement] = failure("while statement is not yet supported")

  def assignment = ((lhs <~ EQ) ~ rhs) ^^ { case l ~ r => ASTAssignment(l, r) }

  def lhs = (identifier
    | identifierWithIndex)

  def rhs = expression

  def expression: Parser[ASTExpression] = arithmeticExpression

  def arithmeticExpression = aexp1

  def aexp1 = aexp2 ~ repsep(aexp2, LOGICAL_OR) ^^ {
    case e ~ Nil => e
    case e ~ l => l.foldLeft(e) { (x, y) => ASTBinaryExpression(x, LogicalOrOp, y) }
  }

  def aexp2 = aexp3 ~ repsep(aexp3, LOGICAL_AND) ^^ {
    case e ~ Nil => e
    case e ~ l => l.foldLeft(e) { (x, y) => ASTBinaryExpression(x, LogicalAndOp, y) }
  }

  def aexp3 = aexp4 ~ repsep(aexp4, BINARY_OR) ^^ {
    case e ~ Nil => e
    case e ~ l => l.foldLeft(e) { (x, y) => ASTBinaryExpression(x, BinaryOrOp, y) }
  }

  def aexp4 = aexp5 ~ repsep(aexp5, BINARY_AND) ^^ {
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
    | stringLiteral )
    
  def functionApplication = identifier ~ LPAREN ~ repsep(expression, COMMA) ~ RPAREN ^^ { case exp~LPAREN~args~RPAREN => ASTFunctionApplication(exp,args) }

  def scalar: Parser[ASTScalar] = integerLiteral | floatingPointLiteral

  def integerLiteral = elem("integer", { case IntegerLiteral(_) => true case _ => false }) ^^ { case IntegerLiteral(i) => ASTInteger(i) }
  def floatingPointLiteral = elem("floating point", { case FloatingPointLiteral(_) => true case _ => false }) ^^ { case FloatingPointLiteral(f) => ASTFloatingPoint(f) }
  def stringLiteral = elem("string", { case StringLiteral(_) => true case _ => false }) ^^ { case StringLiteral(s) => ASTString(s) }

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
}
