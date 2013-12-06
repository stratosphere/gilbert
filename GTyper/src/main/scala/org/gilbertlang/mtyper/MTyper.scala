package org.gilbertlang.mtyper

import org.gilbertlang.mparser.ast.MAst._
import org.gilbertlang.mlibrary.MTypes._
import org.gilbertlang.mlibrary.MTypes.Helper._
import types.MTypedAst._
import org.gilbertlang.mlibrary.MValues.ValueVar
import org.gilbertlang.mlibrary.MOperators._
import org.gilbertlang.mlibrary.MValues.MValue
import org.gilbertlang.mlibrary.MValues.Helper._
import org.gilbertlang.mlibrary.MValues.UniversalValue
import org.gilbertlang.mlibrary.MValues.UndefinedValue
import org.gilbertlang.mtyper.errors.TypingError
import org.gilbertlang.mlibrary.MBuiltinSymbols
import org.gilbertlang.mtyper.errors.TypeNotFoundError
import org.gilbertlang.mlibrary.ConvenienceMethods._
import org.gilbertlang.mlibrary.MValues.ReferenceValue
import org.gilbertlang.mlibrary.MValues.IntValue
import org.gilbertlang.mtyper.errors.TypeNotFoundError
import org.gilbertlang.mtyper.errors.TypingError
import org.gilbertlang.mtyper.errors.NotYetImplementedError
import org.gilbertlang.mtyper.errors.ValueNotFoundError

trait MTyper {
  private val typeEnvironment = scala.collection.mutable.Map[String, MType]()
  private val valueEnvironment = scala.collection.mutable.Map[String, TypedExpression]()
  private val typeVarMapping = scala.collection.mutable.Map[MType, MType]()
  private val valueVarMapping = scala.collection.mutable.Map[MValue, MValue]()

  def resolveType(datatype: MType): MType = {
    datatype match {
      case _: AbstractTypeVar =>
        var result: MType = datatype

        while (result != typeVarMapping.getOrElse(result, result)) {
          result = typeVarMapping(result)
        }

        result match{
          case _:AbstractTypeVar => result
          case _ => resolveType(result)
        }
      case MatrixType(elementType, rowValue, colValue) => MatrixType(resolveType(elementType),
        resolveValue(rowValue), resolveValue(colValue))
      case FunctionType(args, result) => FunctionType(args map { resolveType(_) }, resolveType(result))
      case PolymorphicType(types) => PolymorphicType(types map { resolveType(_) })
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

  def updateValueVarMapping(valueVar: ValueVar, value: MValue) {
    valueVarMapping.update(valueVar, value)
  }

  def resolvePolymorphicType(a: MType, b: MType): Option[(MType, Int)] = {
    a match {
      case PolymorphicType(types) =>
        types.zipWithIndex.toIterator.map {
          case (signature, index) => unify(specializeType(generalizeType(b)), signature) match {
            case Some(_) => unify(b,signature) match{
              case Some(t) => Some(t,index)
              case _ => None
            }
            case _ => None
          }
        } find ({ x: Option[(MType, Int)] => x != None }) flatten
      case _ => unify(b, a) match {
        case Some(t) => Some(t, 0)
        case _ => None
      }
    }
  }

  def resolveValueReferences(datatype: MType, arguments: List[TypedExpression]): MType = {
    datatype match {
      case FunctionType(args, result) => FunctionType(args map { resolveValueReferences(_, arguments) },
        resolveValueReferences(result, arguments))
      case PolymorphicType(types) => PolymorphicType(types map { resolveValueReferences(_, arguments) })
      case MatrixType(elementType, rowValue, colValue) => MatrixType(resolveValueReferences(elementType, arguments),
        resolveValueReferences(rowValue, arguments),
        resolveValueReferences(colValue, arguments))
      case x => x
    }
  }

  def evaluateExpression(expression: TypedExpression): MValue = {
    expression match {
      case TypedInteger(value) => IntValue(value)
      case TypedIdentifier(id,_) => {
        getValue(id) match{
          case Some(t) => evaluateExpression(t)
          case _ => throw new ValueNotFoundError("identifier " + id + " has no value assigned")
        }
      }
      case _ => throw new NotImplementedError("expression evaluation is not yet fully implemented")
    }
  }

  def resolveValueReferences(value: MValue, arguments: List[TypedExpression]): MValue = {
    value match {
      case ReferenceValue(idx) => evaluateExpression(arguments(idx))
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
    val replacementValues = scala.collection.mutable.Map[ValueVar, ValueVar]()
    def helper(a: MType): MType = {
      a match {
        case UniversalType(x: NumericTypeVar) => replacement.getOrElseUpdate(x, newNumericTV())
        case UniversalType(x: TypeVar) => replacement.getOrElseUpdate(x, newTV())
        case MatrixType(elementType, rowValue, colValue) => MatrixType(helper(elementType), helperValue(rowValue),
          helperValue(colValue))
        case PolymorphicType(types) => PolymorphicType(types map { helper(_) })
        case FunctionType(args, result) => FunctionType(args map { helper(_) }, helper(result))
        case x => x
      }
    }
    def helperValue(a: MValue): MValue = {
      a match {
        case UniversalValue(x: ValueVar) => replacementValues.getOrElseUpdate(x, newVV())
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
        case MatrixType(elementType, rowValue, colValue) => MatrixType(helper(elementType), generalizeValue(rowValue),
          generalizeValue(colValue))
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
    else {
      (resolvedValueA, resolvedValueB) match {
        case (x: ValueVar, y) =>
          updateValueVarMapping(x, y)
          Some(y)
        case (x, y: ValueVar) =>
          updateValueVarMapping(y, x)
          Some(x)
        case (UndefinedValue, _) => Some(UndefinedValue)
        case (_ , UndefinedValue) => Some(UndefinedValue)
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

  def extractIdentifiers(program: ASTProgram): Set[ASTIdentifier] = {
    Set()
  }

  def extractIdentifiers(expression: ASTExpression): Set[String] = {
    def helper(exp: ASTExpression): Set[String] = {
      exp match {
        case ASTIdentifier(id) => Set(id)
        case _: ASTInteger | _: ASTFloatingPoint | _: ASTString => Set()
        case ASTUnaryExpression(exp, _) => helper(exp)
        case ASTBinaryExpression(a, _, b) => helper(a) ++ helper(b)
        case ASTAnonymousFunction(parameters, body) => helper(body) --
          (parameters map { case ASTIdentifier(id) => id }).toSet
        case ASTFunctionApplication(function, parameters) => helper(function) ++ (parameters flatMap { helper(_) }).toSet
        case ASTFunctionReference(function) => helper(function)
        case ASTMatrix(rows) => rows flatMap { helper(_) } toSet
        case ASTMatrixRow(exps) => exps flatMap { helper(_) } toSet
      }
    }
    helper(expression)
  }

  def updateEnvironment(identifier: String, datatype: MType) = typeEnvironment.update(identifier, datatype)
  def updateEnvironment(identifier: ASTIdentifier, datatype: MType) = typeEnvironment.update(identifier.value, datatype)
  
  def updateValueEnvironment(identifier: ASTIdentifier, expression: TypedExpression): Unit = {
    updateValueEnvironment(identifier.value,expression)
  }
  
  def updateValueEnvironment(identifier: String, expression: TypedExpression): Unit ={ 
    valueEnvironment.update(identifier, expression)
  }

  def getType(id: String): Option[MType] = {
    MBuiltinSymbols.getType(id) match {
      case None => typeEnvironment.get(id) match {
        case Some(t) => Some(resolveType(t))
        case None => None
      }
      case Some(t) => Some(resolveType(t))
    }
  }
  
  def getValue(id: String): Option[TypedExpression] ={
    valueEnvironment.get(id)
  }

  def extractType(expression: TypedExpression) = expression.datatype

  def typeProgram(program: ASTProgram): TypedProgram = program match {
    case ASTProgram(stmtFuncList) => TypedProgram(stmtFuncList map {
      case stmt: ASTStatement => typeStmt(stmt)
      case func: ASTFunction => typeFunction(func)
      case typeAnnotation: ASTTypeAnnotation => 
        throw new NotYetImplementedError("Type annotations are not yet supported")
    })
  }

  def typeStmt(stmt: ASTStatement): TypedStatement = stmt match {
    case ASTOutputResultStatement(stmt) => TypedOutputResultStatement(typeStmtWithResult(stmt))
    case ASTNOP => TypedNOP
    case x: ASTStatementWithResult => typeStmtWithResult(x)
  }

  def typeStmtWithResult(stmt: ASTStatementWithResult): TypedStatementWithResult = stmt match {
    case ASTAssignment(lhs, rhs) =>
      val typedRHS = typeExpression(rhs)
      updateValueEnvironment(lhs,typedRHS)
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
        case Some((FunctionType(_, resultType), _)) => TypedUnaryExpression(typedExpression, op, resultType)
        case _ => throw new TypeNotFoundError("Unary expression: " + ASTUnaryExpression(exp, op))
      }
    case ASTBinaryExpression(a, op, b) =>
      val typedExpressionA = typeExpression(a)
      val typedExpressionB = typeExpression(b)
      val operatorType = typeBinaryOperator(op)
      val unificationResult = resolvePolymorphicType(operatorType, FunctionType(List(extractType(typedExpressionA),
        extractType(typedExpressionB)), newTV()))

      unificationResult match {
        case Some((FunctionType(_, resultType), _)) => TypedBinaryExpression(typedExpressionA, op, typedExpressionB,
          resultType)
        case _ => throw new TypeNotFoundError("Binary expression: " + ASTBinaryExpression(a, op, b))
      }
    case ASTFunctionApplication(func, arguments) => {
      val typedFunc = typeIdentifier(func)
      val functionType = extractType(typedFunc)
      val typedArguments = arguments map { typeExpression(_) }

      val unificationResult = resolvePolymorphicType(functionType, FunctionType(typedArguments map
        { extractType(_) }, newTV()))

      unificationResult match {
        case Some((FunctionType(_, resultType), _)) =>
          TypedFunctionApplication(typedFunc, typedArguments, resolveValueReferences(resultType, typedArguments))
        case _ => throw new TypeNotFoundError("Function application could not be typed")
      }
    }
    case ASTAnonymousFunction(parameters, body) => {
      val oldMappings = parameters map { case ASTIdentifier(id) => (id, typeEnvironment.get(id)) }
      parameters foreach { case ASTIdentifier(id) => updateEnvironment(id, newTV()) }

      val typedBody = typeExpression(body)

      val typedParameters = parameters map {
        case ASTIdentifier(id) => TypedIdentifier(id, getType(id) match {
          case Some(t) => t
          case _ => throw new TypeNotFoundError("Type for parameter " + id + " could not be found")
        })
      }

      val functionType = FunctionType(typedParameters map { extractType(_) }, extractType(typedBody))
      TypedAnonymousFunction(typedParameters, typedBody, functionType)
    }
    case ASTFunctionReference(function) => {
      val typedIdentifier = typeIdentifier(function)

      typedIdentifier.datatype match {
        case x: FunctionType => TypedFunctionReference(typedIdentifier, typedIdentifier.datatype)
        case _ => throw new TypingError("Identifier " + function.value + " has to be a function type")
      }
    }

    case _ => throw new NotImplementedError("")
  }

  def typeUnaryOperator(operator: UnaryOperator) = operator match {
    case TransposeOp | CellwiseTransposeOp =>
      val (t, a, b) = newTVV()
      PolymorphicType(List(FunctionType(MatrixType(t, a, b), MatrixType(t, b, a)),
        FunctionType(CharacterType, CharacterType),
        FunctionType(IntegerType, IntegerType),
        FunctionType(DoubleType, DoubleType)))
    case PrePlusOp | PreMinusOp =>
      val (t, a, b) = newNTVV()
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
      val (t, a, b) = newNTVV()
      PolymorphicType(List(FunctionType((MatrixType(t, a, b), DoubleType), MatrixType(t, a, b)),
        FunctionType((DoubleType, DoubleType), DoubleType)))
    case PlusOp | MinusOp =>
      val (t, a, b) = newNTVV()
      val (t1, a1, b1) = newNTVV()
      val (t2, a2, b2) = newNTVV()
      PolymorphicType(List(FunctionType((MatrixType(t, a, b), MatrixType(t, a, b)), MatrixType(t, a, b)),
        FunctionType((MatrixType(t1, a1, b1), t1), MatrixType(t1, a1, b1)),
        FunctionType((t2, MatrixType(t2, a2, b2)), MatrixType(t2, a2, b2)),
        FunctionType((IntegerType, IntegerType), IntegerType),
        FunctionType((DoubleType, DoubleType), DoubleType)))
    case MultOp =>
      val (t, a, b) = newNTVV()
      val c = newVV()
      val (t1, a1, b1) = newNTVV()
      val (t2, a2, b2) = newNTVV()
      PolymorphicType(List(FunctionType((MatrixType(t, a, b), MatrixType(t, b, c)), MatrixType(t, a, c)),
        FunctionType((MatrixType(t1, a1, b1), t1), MatrixType(t1, a1, b1)),
        FunctionType((t2, MatrixType(t2, a2, b2)), MatrixType(t2, a2, b2)),
        FunctionType((IntegerType, IntegerType), IntegerType),
        FunctionType((DoubleType, DoubleType), DoubleType)))
    case DivOp =>
      val (t, a, b) = newNTVV()
      PolymorphicType(List(FunctionType((MatrixType(t, a, b), t), MatrixType(t, a, b)),
        FunctionType((DoubleType, DoubleType), DoubleType)))
    case CellwiseMultOp | CellwiseDivOp =>
      val (t, a, b) = newNTVV()
      val (t1, a1, b1) = newNTVV()
      val (t2,a2,b2) = newNTVV()
      PolymorphicType(List(FunctionType((MatrixType(t, a, b), MatrixType(t, a, b)), MatrixType(t, a, b)),
        FunctionType((MatrixType(t1, a1, b1), t1), MatrixType(t1, a1, b1)),
        FunctionType((t2,MatrixType(t2,a2,b2)), MatrixType(t2,a2,b2)),
        FunctionType((IntegerType, IntegerType), IntegerType),
        FunctionType((DoubleType, DoubleType), DoubleType)))
    case LogicalOrOp | LogicalAndOp =>
      PolymorphicType(List(FunctionType((IntegerType, IntegerType), IntegerType),
        FunctionType((DoubleType, DoubleType), DoubleType)))
    case BinaryOrOp | BinaryAndOp =>
      val (t, a, b) = newNTVV()
      val (t1, a1, b1) = newNTVV()
      val (t2, a2, b2) = newNTVV()
      PolymorphicType(List(FunctionType((MatrixType(t, a, b), MatrixType(t, a, b)), MatrixType(IntegerType, a, b)),
        FunctionType((MatrixType(t1, a1, b1), t1), MatrixType(IntegerType, a1, b1)),
        FunctionType((t2, MatrixType(t2, a2, b2)), MatrixType(IntegerType, a2, b2)),
        FunctionType((IntegerType, IntegerType), IntegerType),
        FunctionType((DoubleType, DoubleType), IntegerType)))
    case GTOp | GTEOp | LTOp | LTEOp | DEQOp =>
      val (t, a, b) = newNTVV()
      PolymorphicType(List(FunctionType((DoubleType, DoubleType), DoubleType),
        FunctionType((MatrixType(t, a, b), MatrixType(t, a, b)), MatrixType(IntegerType, a, b))))
  }

  def typeIdentifier(id: ASTIdentifier) = id match {
    case ASTIdentifier(id) =>
      val idType = getType(id) match {
        case Some(t) => t
        case _ => throw new TypeNotFoundError("Identifier " + id + " is unbound")
      }
      TypedIdentifier(id, specializeType(idType))
  }

  def typeFunction(func: ASTFunction) = {
    val typer = new MTyper {}

    func.values foreach { typer.updateEnvironment(_, newTV()) }
    func.parameters foreach { typer.updateEnvironment(_, newTV()) }

    val typedBody = typer.typeProgram(func.body)
    val typedValues = func.values map { typer.typeIdentifier(_) }
    val typedParameters = func.parameters map { typer.typeIdentifier(_) }
    val resultType = if (typedValues.length == 0) VoidType else extractType(typedValues(0))
    val typedFunctionName = TypedIdentifier(func.identifier.value,
      generalizeType(FunctionType(typedParameters map { extractType(_) }, resultType)))

    updateEnvironment(func.identifier, typedFunctionName.datatype)

    TypedFunction(typedValues, typedFunctionName, typedParameters, typedBody)
  }

}