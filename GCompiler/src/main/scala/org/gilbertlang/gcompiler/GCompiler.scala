package org.gilbertlang.gcompiler

import org.gilbertlang._
import org.gilbertlang.gtyper.types.GTypedAst._
import org.gilbertlang.glibrary.ConvenienceMethods
import org.gilbertlang.glibrary.Types.MatrixType
import org.gilbertlang.glibrary.Types.NumericType
import org.gilbertlang.glibrary.Operators._
import errors.TypeCompileError
import org.gilbertlang.glibrary.GBuiltinSymbols
import org.gilbertlang.gtyper.types.GTypedAst
import org.gilbertlang.glibrary.Types.StringType
import org.gilbertlang.glibrary.Types.FunctionType
import org.gilbertlang.glibrary.Types.Type
import org.gilbertlang.gcompiler.errors.ParameterInsertionError
import org.gilbertlang.gcompiler.errors.CompileError
import org.gilbertlang.gcompiler.errors.TypeCompileError

trait GCompiler {
  val assignments = scala.collection.mutable.Map[String, ExpressionExecutable]()
  val functions = scala.collection.mutable.Map[String, Executable]()

  private def addParameter(id: String, datatype: Type, position: Int) {
    datatype match{
      case _:NumericType => assignments.update(id,ScalarParameter(position))
      case _:MatrixType => assignments.update(id, MatrixParameter(position))
      case StringType => assignments.update(id, StringParameter(position))
      case _:FunctionType => assignments.update(id, FunctionParameter(position))
      case _ => throw new ParameterInsertionError("Cannot insert parameter of type " + datatype)
    }
  }
  
  private def registerFunction(functionName: String, executable: Executable) {
    functions.update(functionName, executable)
  }

  private def assign(identifier: String, executable: ExpressionExecutable) {
    assignments.update(identifier, executable)
  }

  private def retrieveExecutable(id: String): ExpressionExecutable = {
    assignments.getOrElse(id, VoidExecutable)
  }

  def compile(typedProgram: TypedProgram): Executable = {
    typedProgram match {
      case TypedProgram(stmtsOrFunctions) =>
        val functions = stmtsOrFunctions collect { case x: TypedFunction => x }
        val stmts = stmtsOrFunctions collect { case x: TypedStatement => x }
        functions.map { compileFunction(_) }.foreach({ (registerFunction _).tupled(_) })
        CompoundExecutable(stmts flatMap { compileStatementWithResult(_) })
    }
  }

  def compileExpression(typedExpression: TypedExpression): ExpressionExecutable = {
    typedExpression match {
      case x: TypedIdentifier => compileIdentifier(x)
      case x: TypedInteger => scalar(x.value)
      case x: TypedFloatingPoint => scalar(x.value)
      case x: TypedString => string(x.value)
      case x: TypedUnaryExpression => compileUnaryExpression(x)
      case x: TypedBinaryExpression => compileBinaryExpression(x)
      case x: TypedFunctionApplication => compileFunctionApplication(x)
      case x: TypedAnonymousFunction => compileAnonymousFunction(x)
      case x: TypedFunctionReference => compileFunctionReference(x)
      case x: TypedMatrix => compileMatrix(x)
    }
  }
  
  def compileMatrix(matrix: TypedMatrix): ExpressionExecutable = {
    VoidExecutable
  }
  
  def compileAnonymousFunction(anonymousFunction: TypedAnonymousFunction): ExpressionExecutable = {
    val oldAssignments = anonymousFunction.parameters map {
      x => retrieveExecutable(x.value) match {
        case VoidExecutable => None
        case y => Some(x.value, y)
      }
    }
    
    (anonymousFunction.parameters zipWithIndex) foreach { case (parameter, idx) => {
      addParameter(parameter.value, parameter.datatype, idx)
    }}
    
    val compiledBody = compileExpression(anonymousFunction.body)
    
    oldAssignments foreach { case Some((id, value)) => assign(id,value) case _ =>}
    
    function(anonymousFunction.parameters.length,compiledBody)
  }
  
  def compileFunctionReference(functionReference: TypedFunctionReference): ExpressionExecutable = {
    compileIdentifier(functionReference.reference)
  }

  def compileIdentifier(identifier: TypedIdentifier): ExpressionExecutable = {
    identifier match {
      case x@ TypedIdentifier(id, datatype) =>
        if (GBuiltinSymbols.isSymbol(id)) {
          compileBuiltInSymbol(id, datatype)
        } else {
          retrieveExecutable(id)
        }
    }
  }

  // TODO: Support of scalar values as well
  def compileBuiltInSymbol(symbol: String, datatype: Type): ExpressionExecutable = {
    symbol match{
      case "load" => function(3,LoadMatrix(StringParameter(0),ScalarParameter(1),
          ScalarParameter(2)))
      case "ones" => function(2, ones(ScalarParameter(0), ScalarParameter(1)))
      case "rand" => function(4, rand(ScalarParameter(0), ScalarParameter(1),
          ScalarParameter(2), ScalarParameter(3)))
      case "binarize" => {
        datatype match {
          case FunctionType(List(_:MatrixType),_) => 
            function(1, CellwiseMatrixTransformation(MatrixParameter(0),Binarize))
          case FunctionType(List(_:NumericType), _) =>
            function(1, UnaryScalarTransformation(ScalarParameter(0), Binarize))
        }
      }
          
      case "maxValue" => {
        datatype match {
          case FunctionType(List(_:MatrixType), _) => 
            function(1, AggregateMatrixTransformation(MatrixParameter(0), Maximum))
          case FunctionType(List(_:NumericType, _:NumericType), _) =>
            function(2, ScalarScalarTransformation(ScalarParameter(0), ScalarParameter(1), Maximum))
        }
      }
      
      case "fixpoint" => {
        function(2, FixpointIteration(MatrixParameter(0), FunctionParameter(1)))
      }
      
      case "spones" => {
        function(1, spones(MatrixParameter(0)))
      }
      
      case "sum" => {
        function(2, sum(MatrixParameter(0), ScalarParameter(1)))
      }
      
      case "sumRow" => {
        function(2, sumRow(MatrixParameter(0), ScalarParameter(1)))
      }
      
      case "sumCol" => {
        function(2, sumCol(MatrixParameter(0), ScalarParameter(1)))
      }
      
      case "diag" => {
        function(1, diag(MatrixParameter(0)))
      }
      
      case "write" => {
        datatype match{
          case FunctionType(List(_:MatrixType, StringType), _) => function(1, WriteMatrix(MatrixParameter(0)))
          case FunctionType(List(_:NumericType, StringType), _) => function(1, WriteScalarRef(ScalarParameter(0)))
          case FunctionType(List(StringType, StringType), _) => function(1, WriteString(StringParameter(0)))
        }
      }
    }
  }

  def compileUnaryExpression(unaryExpression: TypedUnaryExpression) = {
    val exec = compileExpression(unaryExpression.expression)
    
    exec match {
      case x: Matrix => {
        unaryExpression.operator match {
          case PrePlusOp => exec
          case PreMinusOp => CellwiseMatrixTransformation(x, Minus)
          case TransposeOp | CellwiseTransposeOp => Transpose(x)
        }
      }
      case x:ScalarRef => {
        unaryExpression.operator match{
          case PrePlusOp => exec
          case PreMinusOp => UnaryScalarTransformation(x, Minus)
          case TransposeOp | CellwiseTransposeOp => x
        }
      }
      case x:StringRef => throw new NotImplementedError("Unary operation of string is not yet implemented")
      case _:FunctionRef => throw new CompileError("Unary operations on functions are not supported")
      case VoidExecutable => throw new CompileError("UNary operations on VoidExecutable are not supported")
    }
  }

  def compileBinaryExpression(binaryExpression: TypedBinaryExpression) = {
    val a = compileExpression(binaryExpression.a)
    val b = compileExpression(binaryExpression.b)
    
    (a,b) match{
      case (x:Matrix, y:Matrix) =>{
        binaryExpression.operator match{
          case MultOp => MatrixMult(x,y)
          case DivOp | CellwiseDivOp => CellwiseMatrixMatrixTransformation(x,y,Division)
          case PlusOp => CellwiseMatrixMatrixTransformation(x,y,Addition)
          case MinusOp => CellwiseMatrixMatrixTransformation(x,y,Subtraction)
          case CellwiseMultOp => CellwiseMatrixMatrixTransformation(x,y,Multiplication)
          case BinaryAndOp | BinaryOrOp | LogicalAndOp | LogicalOrOp | ExpOp | CellwiseExpOp | GTOp |
          GTEOp | LTOp | LTEOp | DEQOp => {
            throw new NotImplementedError("Operator " + binaryExpression.operator + " is not yet implemented")
          }
        }
      }
      case (x:Matrix, y: ScalarRef) => {
        binaryExpression.operator match{
          case MultOp | CellwiseMultOp => MatrixScalarTransformation(x,y,Multiplication)
          case DivOp | CellwiseDivOp => MatrixScalarTransformation(x,y,Division)
          case PlusOp => MatrixScalarTransformation(x,y,Addition)
          case MinusOp => MatrixScalarTransformation(x,y,Subtraction)
          case BinaryAndOp | BinaryOrOp | LogicalAndOp | LogicalOrOp | ExpOp | CellwiseExpOp | GTOp |
          GTEOp | LTOp | LTEOp | DEQOp => {
            throw new NotImplementedError("Operator " + binaryExpression.operator + " is not yet implemented")
          }
        }
      }
      case (x:ScalarRef, y: Matrix) => {
        binaryExpression.operator match{
          case MultOp | CellwiseMultOp => ScalarMatrixTransformation(x,y,Multiplication)
          case DivOp | CellwiseDivOp => ScalarMatrixTransformation(x,y,Division)
          case PlusOp => ScalarMatrixTransformation(x,y,Addition)
          case MinusOp => ScalarMatrixTransformation(x,y,Subtraction)
          case BinaryAndOp | BinaryOrOp | LogicalAndOp | LogicalOrOp | ExpOp | CellwiseExpOp | GTOp |
          GTEOp | LTOp | LTEOp | DEQOp => {
            throw new NotImplementedError("Operator " + binaryExpression.operator + " is not yet implemented")
          } 
        }
      }
      case (x: ScalarRef, y: ScalarRef) => {
        binaryExpression.operator match{
          case MultOp | CellwiseMultOp => ScalarScalarTransformation(x,y,Multiplication)
          case DivOp | CellwiseDivOp => ScalarScalarTransformation(x,y,Division)
          case PlusOp => ScalarScalarTransformation(x,y,Addition)
          case MinusOp => ScalarScalarTransformation(x,y,Subtraction)
          case BinaryAndOp | BinaryOrOp | LogicalAndOp | LogicalOrOp | ExpOp | CellwiseExpOp | GTOp |
          GTEOp | LTOp | LTEOp | DEQOp => {
            throw new NotImplementedError("Operator " + binaryExpression.operator + " is not yet implemented")
          } 
        }
      }
      case (x: StringRef, y: StringRef) =>
        throw new NotImplementedError("Binary operation for 2 strings is not yet implemented")
      case (x: StringRef, y:Matrix) =>
        throw new NotImplementedError("Binary operation for string and matrix is not supported")
      case (x: StringRef, y:ScalarRef) =>
        throw new NotImplementedError("Binary operation for string and scalar is not yet implemented")
      case (x: Matrix, y: StringRef) =>
        throw new NotImplementedError("BinaryOperation for matrix and string is not supported")
      case (x: ScalarRef, y: StringRef) =>
        throw new NotImplementedError("BinaryOperation for scalar and string is not supported")
      case (_: FunctionRef, _) | (_, _:FunctionRef) =>
        throw new CompileError("Binary operation on a function is not supported")
      case (VoidExecutable, _) | (_ , VoidExecutable) =>
        throw new CompileError("Binary operation on VoidExecutable is not supported")
    }
    
  }

  def compileFunctionApplication(functionApplication: TypedFunctionApplication) = {
    val fun = compileIdentifier(functionApplication.id)
    val arguments = functionApplication.args map { compileExpression(_)}
    
    fun match {
      case function(numParameters, body) if numParameters <= arguments.length => body.instantiate(arguments:_*) match {
        case x:ExpressionExecutable => x
        case _ => throw new TypeCompileError("Return value of a function has to be an expression")
      }
      case _ => throw new TypeCompileError("Id has to be of a function type")
    }
  }

  // TODO: Handle return values of functions
  def compileFunction(typedFunction: TypedFunction): (String, FunctionRef) = {
    val compiler = new GCompiler {}
    (typedFunction.parameters zipWithIndex) foreach ({ case (TypedIdentifier(id,datatype),position) => 
      compiler.addParameter(id, datatype,position) })
    val compiledFunction = compiler.compile(typedFunction.body)
    
    (typedFunction.identifier.value, function(typedFunction.parameters.length, compiledFunction))
  }

  def compileStatementWithResult(typedStatement: TypedStatement): Option[Executable] = {
    typedStatement match {
      case TypedAssignment(lhs, rhs) =>
        val result = compileExpression(rhs)
        assign(lhs.value, result)
        None
      case x: TypedExpression =>
        compileExpression(x)
        None
      case TypedNOP => None
      case TypedOutputResultStatement(stmt) =>
        GTypedAst.getType(stmt) match {
          case _: MatrixType =>
            compileStatement(stmt) match {
              case x: Matrix => Some(WriteMatrix(x))
              case _ => throw new TypeCompileError("Expected executable of type Matrix")
            }
          case _: NumericType =>
            compileStatement(stmt) match {
              case x: ScalarRef => Some(WriteScalarRef(x))
              case _ => throw new TypeCompileError("Expected executable of type ScalarRef")
            }
          case StringType => 
            compileStatement(stmt) match {
              case x:StringRef => Some(WriteString(x))
              case _ => throw new TypeCompileError("Expected executable of type StringRef")
            }
          case _ => None
        }
    }
  }

  def compileStatement(typedStatement: TypedStatement): Executable = {
    typedStatement match {
      case TypedAssignment(lhs, rhs) =>
        val result = compileExpression(rhs)
        assign(lhs.value, result)
        result
      case x: TypedExpression => compileExpression(x)
      case TypedNOP => VoidExecutable
      case TypedOutputResultStatement(stmt) =>
        GTypedAst.getType(stmt) match {
          case _: MatrixType =>
            compileStatement(stmt) match {
              case x: Matrix => WriteMatrix(x)
              case _ => throw new TypeCompileError("Expected executable of type Matrix")
            }
          case _: NumericType =>
            compileStatement(stmt) match {
              case x: ScalarRef => WriteScalarRef(x)
              case _ => throw new TypeCompileError("Expected executable of type ScalarRef")
            }
          case StringType => {
            compileStatement(stmt) match {
              case x: StringRef => WriteString(x)
              case _ => throw new TypeCompileError("Expected executable of type StringRef")
            }
          }
          case _ => VoidExecutable
        }
    }
  }
}