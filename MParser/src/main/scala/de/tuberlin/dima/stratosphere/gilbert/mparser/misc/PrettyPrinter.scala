package de.tuberlin.dima.stratosphere.gilbert.mparser.misc

import de.tuberlin.dima.stratosphere.gilbert.mparser.ast.MAst._

object PrettyPrinter {
	def prettyPrint(program: ASTProgram):Unit = {
	  printProgram(program,0)
	}
	
	def printProgram(program: ASTProgram, indent: Int): Unit = {
	  program match {
	    case ASTProgram(program) => 
	      val indentation = " "*indent
	      println(indentation + "ASTProgram(")
	      program foreach (printStmtOrFunc(_,indent+1))
	      println(indentation + ")")
	  }
	}
	
	def printStmtOrFunc(stmtOrFunc:ASTStatementOrFunction, indent:Int) = {
	  stmtOrFunc match {
	    case x:ASTFunction => printFunction(x,indent)
	    case x:ASTStatement => printStatement(x,indent)
	  }
	}
	
	def printFunction(function:ASTFunction, indent:Int) = {
	  val indentation =  " "*indent
	  println(indentation + "Not yet implemented")
	}
	
	def printStatement(stmt:ASTStatement, indent:Int) ={
	  val indentation = " "*indent
	  stmt match {
	    case ASTAssignment(lhs, rhs) => 
	      println(indentation + "ASTAssignment(")
	      printExpression(lhs,indent+1)
	      printExpression(rhs,indent+1)
	      println(indentation + ")")
	    case x:ASTExpression => printExpression(x,indent)
	  }
	}
	
	def printExpression(exp: ASTExpression, indent:Int):Unit ={
	  val indentation = " "*indent
	  exp match{
	    case ASTUnaryExpression(exp,op) => 
	      println(indentation + "ASTUnaryExpression(")
	      printExpression(exp,indent+1)
	      println(indentation+" " + op)
	      println(indentation+")")
	    case ASTBinaryExpression(a,op,b) =>
	      println(indentation + "ASTBinaryExpression(")
	      printExpression(a,indent+1)
	      println(indentation+" "+op)
	      printExpression(b,indent+1)
	      println(indentation+")")
	    case ASTFunctionApplication(func,args) =>
	      println(indentation + "ASTFunctionApplication(");
	      printExpression(func,indent+1)
	      println(indentation + " (");
	      args foreach { printExpression(_,indent+1)}
	      println(indentation+ " )");
	      println(indentation+")")
	    case _ => println(indentation + exp)   
	  }
	}
}