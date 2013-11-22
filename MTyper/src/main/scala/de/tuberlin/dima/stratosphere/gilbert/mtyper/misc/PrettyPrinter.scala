package de.tuberlin.dima.stratosphere.gilbert.mtyper.misc

import de.tuberlin.dima.stratosphere.gilbert.mtyper.types.MTypedAst._

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
	  println(indentation + "Not yet implemented")
	}
	
	def printStatement(stmt:TypedStatement, indent:Int) ={
	  val indentation = " "*indent
	  stmt match {
	    case TypedAssignment(lhs, rhs) => 
	      println(indentation + "TypedAssignment(")
	      printExpression(lhs,indent+1)
	      printExpression(rhs,indent+1)
	      println(indentation + ")")
	    case x:TypedExpression => printExpression(x,indent)
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
	    case _ => println(indentation + exp)   
	  }
	}
}