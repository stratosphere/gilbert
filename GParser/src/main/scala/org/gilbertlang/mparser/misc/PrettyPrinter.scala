package org.gilbertlang.mparser.misc

import org.gilbertlang.mparser.ast.MAst._

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
	    case x:ASTTypeAnnotation => printAnnotation(x,indent)
	  }
	}
	
	def printAnnotation(annotation: ASTTypeAnnotation, indent: Int){
	  val indentation = " "*indent
	  println(indentation + annotation)
	}
	
	def printFunction(function:ASTFunction, indent:Int) = {
	  val indentation =  " "*indent
	  val indentation1 = indentation + " ";
	  val indentation2 = indentation1+ " ";
	  println(indentation + "ASTFunction(");
	  println(indentation1 + "Result(");
	  function.values foreach {x => println(indentation2 + x)}
	  println(indentation1 + ")");
	  println(indentation1 + "Name(")
	  println(indentation2 + function.identifier);
	  println(indentation1 + ")")
	  println(indentation1 + "Parameters(");
	  function.parameters foreach { x => println(indentation2 + x )}
	  println(indentation1 + ")");
	  println(indentation1 + "Body(");
	  printProgram(function.body,indent+2);
	  println(indentation1 + ")");
	  println(indentation + ")")
	}
	
	def printStatement(stmt:ASTStatement, indent:Int):Unit ={
	  val indentation = " "*indent
	  stmt match {
	    case ASTAssignment(lhs, rhs) => 
	      println(indentation + "ASTAssignment(")
	      printExpression(lhs,indent+1)
	      printExpression(rhs,indent+1)
	      println(indentation + ")")
	    case x:ASTExpression => printExpression(x,indent)
	    case ASTOutputResultStatement(stmt) =>
	      println(indentation + "ASTOutputResultStatement(")
	      printStatement(stmt,indent+1)
	      println(indentation + ")")
	    case ASTNOP =>
	      println(indentation + "NOP")
	  }
	}
	
	def printExpression(exp: ASTExpression, indent:Int):Unit ={
	  val indentation = " "*indent
	  val indentation1 = " " + indentation
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
	    case ASTAnonymousFunction(parameters, expression) =>
	      println(indentation + "ASTAnonymousFunction(");
	      println(indentation1 + "Parameters(");
	      parameters foreach { printExpression(_, indent+2)}
	      println(indentation1 + ")")
	      println(indentation1 +"Body(")
	      printExpression(expression,indent+2)
	      println(indentation1+")")
	      println(indentation + ")")
	    case _ => println(indentation + exp)   
	  }
	}
}