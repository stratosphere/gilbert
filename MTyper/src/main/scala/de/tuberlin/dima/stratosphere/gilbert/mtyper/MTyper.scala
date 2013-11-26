package de.tuberlin.dima.stratosphere.gilbert.mtyper

import de.tuberlin.dima.stratosphere.gilbert.mparser.ast.MAst._
import types.MTypes._
import types.MTypedAst._
import de.tuberlin.dima.stratosphere.gilbert.mtyper.types.MValues.ValueVar
import de.tuberlin.dima.stratosphere.gilbert.mparser.ast.MOperators._
import de.tuberlin.dima.stratosphere.gilbert.mtyper.types.MValues.MValue
import types.MTypes.Helper._
import de.tuberlin.dima.stratosphere.gilbert.mtyper.types.MValues.UniversalValue
import types.MValues.Helper._
import de.tuberlin.dima.stratosphere.gilbert.mtyper.errors.TypingError
import builtin.BuiltInSymbols.builtInSymbols
import de.tuberlin.dima.stratosphere.gilbert.mtyper.errors.TypeNotFoundError
import types.ConvenienceMethods._
import de.tuberlin.dima.stratosphere.gilbert.mtyper.types.MValues.ReferenceValue
import de.tuberlin.dima.stratosphere.gilbert.mtyper.types.MValues.ExpressionValue

trait MTyper {
  private val typeEnvironment = scala.collection.mutable.Map[String, MType]()
  private val typeVarMapping = scala.collection.mutable.Map[MType, MType]()
  private val valueVarMapping = scala.collection.mutable.Map[MValue, MValue]()

  def resolveType(datatype: MType): MType = {
    datatype match {
      case _: AbstractTypeVar =>
        var result: MType = datatype

        while (result != typeVarMapping.getOrElse(result, result)) {
          result = typeVarMapping(result)
        }

        result
      case MatrixType(elementType, rowValue, colValue) => MatrixType(resolveType(elementType),
          resolveValue(rowValue), resolveValue(colValue))
      case FunctionType(args,result) => FunctionType(args map {resolveType(_)},resolveType(result))
      case PolymorphicType(types) => PolymorphicType(types map {resolveType(_)})
      case x => x
    }
  }

  def simplifyValue(value: MValue) = value

  def resolveValue(value: MValue): MValue = {
    val resolvedValue = value match {
      case _: ValueVar =>
        var result: MValue = value

        while (result != valueVarMapping.getOrElse(result, result)) {
          result = valueVarMapping(result)
        }

        result
      case _ => value
    }

    simplifyValue(resolvedValue)
  }

  def updateTypeVarMapping(typeVar: AbstractTypeVar, datatype: MType) {
    typeVarMapping.update(typeVar, datatype)
  }
  
  def updateValueVarMapping(valueVar: ValueVar, value: MValue){
    valueVarMapping.update(valueVar,value)
  }
  
  def resolvePolymorphicType(a: MType, b: MType):Option[(MType,Int)] ={
    a match{
      case PolymorphicType(types) => 
        types.zipWithIndex.toIterator.map{
          case (signature,index) => unify(signature,b) match{
            case Some(t) => Some(t,index)
            case _ => None
          }
        }.find { x => x!= None }.flatten
      case _ => unify(a,b) match{
        case Some(t) => Some(t,0)
        case _ => None
      }
    }
  }
  
  def resolveValueReferences(datatype: MType, arguments: List[TypedExpression]):MType = {
    datatype match{
      case FunctionType(args, result) => FunctionType(args map {resolveValueReferences(_,arguments)},resolveValueReferences(result,arguments))
      case PolymorphicType(types) => PolymorphicType(types map { resolveValueReferences(_,arguments)})
      case MatrixType(elementType, rowValue, colValue) => MatrixType(resolveValueReferences(elementType,arguments),
          resolveValueReferences(rowValue,arguments),
          resolveValueReferences(colValue,arguments))
      case x => x
    }
  }
  
  def resolveValueReferences(value: MValue, arguments: List[TypedExpression]): MValue = {
    value match{
      case ReferenceValue(idx) => ExpressionValue(arguments(idx))
      case x => x
    }
  }

  def widenTypes(a: MType, b: MType): (MType, MType) = {
    (a.isWideableTo(b), b.isWideableTo(a)) match {
      case (true, _) => (b, b)
      case (_, true) => (a, a)
      case _ => (a, b)
    }
  }

  def specializeType(datatype: MType): MType = {
    val replacement = scala.collection.mutable.Map[AbstractTypeVar, AbstractTypeVar]()
    def helper(a: MType): MType = {
      a match {
        case UniversalType(x: NumericTypeVar) => replacement.getOrElseUpdate(x, newNumericTV())
        case UniversalType(x: TypeVar) => replacement.getOrElseUpdate(x, newTV())
        case MatrixType(elementType, rowValue, colValue) => MatrixType(helper(elementType), specializeValue(rowValue), specializeValue(colValue))
        case PolymorphicType(types) => PolymorphicType(types map { helper(_) })
        case FunctionType(args, result) => FunctionType(args map { helper(_) }, helper(result))
        case x => x
      }
    }
    helper(datatype)
  }

  def specializeValue(value: MValue): MValue = {
    val replacement = scala.collection.mutable.Map[ValueVar, ValueVar]()
    def helper(a: MValue): MValue = {
      a match {
        case UniversalValue(x: ValueVar) => replacement.getOrElseUpdate(x, newVV())
        case x => x
      }
    }
    helper(value)
  }

  def freeVariables(datatype: MType): Set[MType] = {
    def helper(a: MType): Set[MType] = {
      a match {
        case UniversalType(x: AbstractTypeVar) => Set()
        case UniversalType(_) => throw new TypingError("Universal cannot be applied to a non type variable")
        case x: AbstractTypeVar => Set(x)
        case MatrixType(elementType, rowValue, colValue) => helper(elementType)
        case PolymorphicType(types) => (types flatMap (helper(_))).toSet
        case FunctionType(args, result) => (args flatMap (helper(_))).toSet ++ helper(result)
        case _ => Set()
      }
    }
    helper(datatype)
  }

  def generalizeType(datatype: MType): MType = {
    val freeVars = typeEnvironment.values.flatMap({ freeVariables(_) }).toSet
    def helper(datatype: MType): MType = {
      datatype match {
        case x @ UniversalType(_: AbstractTypeVar) => x
        case UniversalType(_) => throw new TypingError("Universal cannot be applied to a non type variable")
        case x: AbstractTypeVar => if (freeVars contains x) x else UniversalType(x)
        case MatrixType(elementType, rowValue, colValue) => MatrixType(helper(elementType), generalizeValue(rowValue), generalizeValue(colValue))
        case PolymorphicType(types) => PolymorphicType(types map { helper(_) })
        case FunctionType(args, result) => FunctionType(args map { helper(_) }, helper(result))
        case x => x
      }
    }
    helper(datatype)
  }

  def generalizeValue(value: MValue): MValue = {
    value match {
      case x: ValueVar => UniversalValue(x)
      case x => x
    }
  }

  def unifyValue(a: MValue, b: MValue): Option[MValue] = {
    val resolvedValueA = resolveValue(a)
    val resolvedValueB = resolveValue(b)

    if (resolvedValueA == resolvedValueB) Some(resolvedValueA)
    else{
      (resolvedValueA,resolvedValueB) match{
        case (x:ValueVar,y) => 
          updateValueVarMapping(x, y)
          Some(y)
        case (x, y:ValueVar) =>
          updateValueVarMapping(y,x)
          Some(x)
        case _ => None
      }
    }
  }

  //TODO: type variable contained in other type as subexpression
  def unify(a: MType, b: MType): Option[MType] = {
    val resolvedTypeA = resolveType(a)
    val resolvedTypeB = resolveType(b)

    val (typeA, typeB) = widenTypes(resolvedTypeA, resolvedTypeB)

    if (typeA == typeB) {
      Some(typeA)
    } else {
      (typeA, typeB) match {
        case (x: TypeVar, _) =>
          updateTypeVarMapping(x, typeB)
          Some(typeB)
        case (_, y: TypeVar) =>
          updateTypeVarMapping(y, typeA)
          Some(typeA)
        case (x: NumericTypeVar, y: NumericType) =>
          updateTypeVarMapping(x, y)
          Some(y)
        case (x: NumericType, y: NumericTypeVar) =>
          updateTypeVarMapping(y, x)
          Some(x)
        case (FunctionType(args1, result1), FunctionType(args2, result2)) =>
          if (args1.length != args2.length) None
          else {
            val unifiedArgs = (for ((x, y) <- (args1 zip args2)) yield {
              unify(x, y)
            }) flatMap { x => x }
            val unifiedResult = unify(result1, result2)

            unifiedResult match {
              case Some(resultType) if unifiedArgs.length == args1.length => Some(FunctionType(unifiedArgs, resultType))
              case _ => None
            }
          }
        case (MatrixType(matrixType1, rows1, cols1), MatrixType(matrixType2, rows2, cols2)) =>
          val unifiedType = unify(matrixType1, matrixType2)
          val unifiedRows = unifyValue(rows1, rows2)
          val unifiedCols = unifyValue(cols1, cols2)

          unifiedType match {
            case Some(t) => unifiedRows match {
              case Some(r) => unifiedCols match {
                case Some(c) => Some(MatrixType(t, r, c))
                case _ => None
              }
              case _ => None
            }
            case _ => None
          }
        case (PolymorphicType(typesA), PolymorphicType(typesB)) => None
        case _ => None
      }
    }
  }

  def updateEnvironment(identifier: String, datatype: MType) = typeEnvironment.update(identifier, datatype)
  def updateEnvironment(identifier: ASTIdentifier, datatype: MType) = typeEnvironment.update(identifier.value, datatype)

  def getType(id: String): Option[MType] = typeEnvironment.get(id)

  def extractType(expression: TypedExpression) = expression.datatype

  def typeProgram(program: ASTProgram) = program match {
    case ASTProgram(stmtFuncList) => TypedProgram(stmtFuncList map {
      case stmt: ASTStatement => typeStmt(stmt)
      case func: ASTFunction => typeFunction(func)
    })
  }

  def typeStmt(stmt: ASTStatement): TypedStatement = stmt match {
    case ASTOutputResultStatement(stmt) => TypedOutputResultStatement(typeStmtWithResult(stmt))
    case ASTNOP => TypedNOP
    case x:ASTStatementWithResult => typeStmtWithResult(x)
  }
  
  def typeStmtWithResult(stmt: ASTStatementWithResult): TypedStatementWithResult =  stmt match {
    case ASTAssignment(lhs, rhs) =>
      val typedRHS = typeExpression(rhs)
      updateEnvironment(lhs, extractType(typedRHS))
      TypedAssignment(typeIdentifier(lhs), typedRHS)
    case exp: ASTExpression => typeExpression(exp)
  }

  def typeExpression(exp: ASTExpression): TypedExpression = exp match {
    case id: ASTIdentifier => typeIdentifier(id)
    case ASTInteger(value) => TypedInteger(value)
    case ASTFloatingPoint(value) => TypedFloatingPoint(value)
    case ASTString(value) => TypedString(value)
    case ASTUnaryExpression(exp, op) =>
      val typedExpression = typeExpression(exp)
      val operatorType = typeUnaryOperator(op)
      val unificationResult = resolvePolymorphicType(operatorType, FunctionType(extractType(typedExpression), newTV()))

      unificationResult match {
        case Some((FunctionType(_,resultType), _)) => TypedUnaryExpression(typedExpression, op, resultType)
        case _ => throw new TypeNotFoundError("Unary expression: " + ASTUnaryExpression(exp, op))
      }
    case ASTBinaryExpression(a, op, b) =>
      val typedExpressionA = typeExpression(a)
      val typedExpressionB = typeExpression(b)
      val operatorType = typeBinaryOperator(op)
      val unificationResult = resolvePolymorphicType(operatorType, FunctionType(List(extractType(typedExpressionA), extractType(typedExpressionB)), newTV()))

      unificationResult match {
        case Some((FunctionType(_,resultType), _)) => TypedBinaryExpression(typedExpressionA, op, typedExpressionB, resultType)
        case _ => throw new TypeNotFoundError("Binary expression: " + ASTBinaryExpression(a, op, b))
      }
    case ASTFunctionApplication(func, arguments) =>
      val typedFunc = typeIdentifier(func)
      val functionType = extractType(typedFunc)
      val typedArguments = arguments map {typeExpression(_)}
      
      val unificationResult = resolvePolymorphicType(functionType,FunctionType(typedArguments map {extractType(_)},newTV()))
      
      unificationResult match{
        case Some((FunctionType(_,resultType),_)) => 
          TypedFunctionApplication(typedFunc,typedArguments,resolveValueReferences(resultType,typedArguments))
        case _ => throw new TypeNotFoundError("Function application could not be typed")
      }

    case _ => throw new NotImplementedError("")
  }

  def typeUnaryOperator(operator: UnaryOperator) = operator match {
    case TransposeOp | CellwiseTransposeOp =>
      val(t,a,b) = newTVV()
      PolymorphicType(List(FunctionType(MatrixType(t, a, b), MatrixType(t, b, a)),
        FunctionType(CharacterType, CharacterType),
        FunctionType(IntegerType, IntegerType),
        FunctionType(DoubleType, DoubleType)))
    case PrePlusOp | PreMinusOp =>
      val(t,a,b) = newNTVV()
      PolymorphicType(List(FunctionType(MatrixType(t, a, b), MatrixType(t, a, b)),
        FunctionType(IntegerType, IntegerType),
        FunctionType(DoubleType, DoubleType)))
  }

  def typeBinaryOperator(operator: BinaryOperator): MType = operator match {
    case ExpOp =>
      val a = newVV()
      val t = newNumericTV()
      PolymorphicType(List(FunctionType((MatrixType(t, a, a), DoubleType), MatrixType(t, a, a)),
        FunctionType((DoubleType, DoubleType), DoubleType)))
    case CellwiseExpOp =>
      val(t,a,b) = newNTVV()
      PolymorphicType(List(FunctionType((MatrixType(t, a, b), DoubleType), MatrixType(t, a, b)),
        FunctionType((DoubleType, DoubleType), DoubleType)))
    case PlusOp | MinusOp =>
      val(t,a,b) = newNTVV()
      val(t1,a1,b1) = newNTVV()
      val(t2,a2,b2) = newNTVV()
      PolymorphicType(List(FunctionType((MatrixType(t, a, b), MatrixType(t, a, b)), MatrixType(t, a, b)),
        FunctionType((MatrixType(t1, a1, b1), t1), MatrixType(t1, a1, b1)),
        FunctionType((t2, MatrixType(t2, a2, b2)), MatrixType(t2, a2, b2)),
        FunctionType((IntegerType, IntegerType), IntegerType),
        FunctionType((DoubleType, DoubleType), DoubleType)))
    case MultOp =>
      val(t,a,b) = newNTVV()
      val c = newVV()
      val(t1,a1,b1) = newNTVV()
      val(t2,a2,b2) = newNTVV()
      PolymorphicType(List(FunctionType((MatrixType(t, a, b), MatrixType(t, b, c)), MatrixType(t, a, c)),
        FunctionType((MatrixType(t1, a1, b1), t1), MatrixType(t1, a1, b1)),
        FunctionType((t2, MatrixType(t2, a2, b2)), MatrixType(t2, a2, b2)),
        FunctionType((IntegerType, IntegerType), IntegerType),
        FunctionType((DoubleType, DoubleType), DoubleType)))
    case DivOp =>
      val(t,a,b) = newNTVV()
      PolymorphicType(List(FunctionType((MatrixType(t, a, b), t), MatrixType(t, a, b)),
        FunctionType((DoubleType, DoubleType), DoubleType)))
    case CellwiseMultOp | CellwiseDivOp =>
      val(t,a,b) = newNTVV()
      val(t1,a1,b1) = newNTVV()
      PolymorphicType(List(FunctionType((MatrixType(t, a, b), MatrixType(t, a, b)), MatrixType(t, a, b)),
        FunctionType((MatrixType(t1, a1, b1), t1), MatrixType(t1, a1, b1)),
        FunctionType((IntegerType, IntegerType), IntegerType),
        FunctionType((DoubleType, DoubleType), DoubleType)))
    case LogicalOrOp | LogicalAndOp =>
      PolymorphicType(List(FunctionType((IntegerType, IntegerType), IntegerType),
        FunctionType((DoubleType, DoubleType), DoubleType)))
    case BinaryOrOp | BinaryAndOp =>
      val(t,a,b) = newNTVV()
      val(t1,a1,b1) = newNTVV()
      val(t2,a2,b2) = newNTVV()
      PolymorphicType(List(FunctionType((MatrixType(t, a, b), MatrixType(t, a, b)), MatrixType(IntegerType, a, b)),
        FunctionType((MatrixType(t1, a1, b1), t1), MatrixType(IntegerType, a1, b1)),
        FunctionType((t2, MatrixType(t2, a2, b2)), MatrixType(IntegerType, a2, b2)),
        FunctionType((IntegerType, IntegerType), IntegerType),
        FunctionType((DoubleType, DoubleType), IntegerType)))
    case GTOp | GTEOp | LTOp | LTEOp | DEQOp =>
      val(t,a,b) = newNTVV()
      PolymorphicType(List(FunctionType((DoubleType, DoubleType), DoubleType),
        FunctionType((MatrixType(t, a, b), MatrixType(t, a, b)), MatrixType(IntegerType, a, b))))
  }

  def typeIdentifier(id: ASTIdentifier) = id match {
    case ASTIdentifier(id) =>
      val idType = builtInSymbols.getOrElse(id, getType(id) match {
        case Some(t) => t
        case _ => throw new TypeNotFoundError("Identifier " + id + " is unbound")
      })
      TypedIdentifier(id, specializeType(idType))
  }

  def typeFunction(func: ASTFunction) = throw new NotImplementedError("Function typing is not yet supported")
}