package org.gilbertlang.mtyper.misc

import org.gilbertlang.mtyper.types.MTypedAst._
import org.gilbertlang.mlibrary.MTypes.MType
import org.gilbertlang.mlibrary.MTypes._

object PrettyPrinter {
	def prettyPrint(program: TypedProgram):Unit = {
	  printProgram(program,0)
	}
	
	def printProgram(program: TypedProgram, indent: Int): Unit = {
	  program match {
	    case TypedProgram(program) => 
	      val indentation = " "*indent
	      println(indentation + "TypedProgram(")
	      program foreach (printStmtOrFunc(_,indent+1))
	      println(indentation + ")")
	  }
	}
	
	def printStmtOrFunc(stmtOrFunc:TypedStatementOrFunction, indent:Int) = {
	  stmtOrFunc match {
	    case x:TypedFunction => printFunction(x,indent)
	    case x:TypedStatement => printStatement(x,indent)
	  }
	}
	
	def printFunction(function:TypedFunction, indent:Int) = {
	  val indentation =  " "*indent
	  val indentation1 = " "+indentation
	  val indentation2 = " "+indentation1
	  function match{
	    case TypedFunction(values, id, parameters, body) => {
	      println(indentation + "TypedFunction(");
	      println(indentation1 + "Name(");
	      println(indentation2 + id.value)
	      println(indentation1 + ")");
	      printType(id.datatype,indent+2)
	      printProgram(body,indent+2)
	      println(indentation + ")");
	    }
	  }
	}
	
	def printType(datatype: MType, indent: Int) {
	  val indentation = " "*indent
	  val indentation1 =" " + indentation
	  datatype match{
	    case IntegerType => println(indentation + "Int")
	    case FunctionType(parameters, result) => {
	      println(indentation + "Parameters(")
	      parameters foreach { printType(_,indent+1)}
	      println(indentation + ")");
	      println(indentation + "Result(")
	      printType(result,indent+1)
	      println(indentation + ")")
	    }
	    case MatrixType(elementType,rows,cols) => {
	      println(indentation + "Matrix(")
	      printType(elementType,indent+1)
	      println(indentation1 + rows)
	      println(indentation1 + cols)
	      println(indentation + ")")
	    }
	    case x => println(indentation + x)
	  }
	}
	
	def printStatement(stmt:TypedStatement, indent:Int):Unit ={
	  val indentation = " "*indent
	  stmt match {
	    case TypedAssignment(lhs, rhs) => 
	      println(indentation + "TypedAssignment(")
	      printExpression(lhs,indent+1)
	      printExpression(rhs,indent+1)
	      println(indentation + ")")
	    case x:TypedExpression => printExpression(x,indent)
	    case TypedNOP => println(indentation + "NOP")
	    case TypedOutputResultStatement(stmt) => 
	      println(indentation + "TypedOutputResultStatement(")
	      printStatement(stmt, indent+1)
	      println(indentation + ")")
	  }
	}
	
	def printExpression(exp: TypedExpression, indent:Int):Unit ={
	  val indentation = " "*indent
	  exp match{
	    case TypedUnaryExpression(exp,op,resultType) => 
	      println(indentation + "TypedUnaryExpression(")
	      printExpression(exp,indent+1)
	      println(indentation+" " + op)
	      println(indentation+"):"+resultType)
	    case TypedBinaryExpression(a,op,b,resultType) =>
	      println(indentation + "TypedBinaryExpression(")
	      printExpression(a,indent+1)
	      println(indentation+" "+op)
	      printExpression(b,indent+1)
	      println(indentation+"):"+resultType)
	    case TypedFunctionApplication(func,args,resultType) =>
	      println(indentation + "TypedFunctionApplication(");
	      printExpression(func,indent+1)
	      println(indentation + " (");
	      args foreach { printExpression(_,indent+1)}
	      println(indentation+ " )");
	      println(indentation+"):"+resultType)
	    case TypedIdentifier(id,datatype) => println(indentation + id + ":" + datatype)
	    case x => println(indentation + x)
	  }
	}
}