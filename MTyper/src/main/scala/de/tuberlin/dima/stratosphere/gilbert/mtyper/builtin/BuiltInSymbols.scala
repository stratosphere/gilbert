package de.tuberlin.dima.stratosphere.gilbert.mtyper.builtin

import de.tuberlin.dima.stratosphere.gilbert.mtyper.types.MTypes._
import de.tuberlin.dima.stratosphere.gilbert.mtyper.types.MTypes.Helper._
import de.tuberlin.dima.stratosphere.gilbert.mtyper.types.MValues._
import de.tuberlin.dima.stratosphere.gilbert.mtyper.types.MValues.Helper._
import de.tuberlin.dima.stratosphere.gilbert.mtyper.types.ConvenienceMethods._

object BuiltInSymbols {
  val builtInSymbols = scala.collection.immutable.Map[String, MType](
    "load" -> getLoadType,
    "bin" -> getBinType,
    "maxValue" -> getMaxValueType)

  def getLoadType = FunctionType((MatrixType(CharacterType, IntValue(1), newVV()), IntegerType, IntegerType), MatrixType(DoubleType, ReferenceValue(1), ReferenceValue(2)))
  
  def getBinType = {
    val (t,a,b) = newNTVV()
    val numericType = newNumericTV()
    PolymorphicType(List(
    FunctionType(MatrixType(t,a,b),MatrixType(IntegerType,a,b)),
    FunctionType(numericType,IntegerType)))
  }
  
  def getMaxValueType = {
    val (t,a,b) = newNTVV()
    val numericType = newNumericTV()
    PolymorphicType(List(
        FunctionType(MatrixType(t,a,b),t),
        FunctionType(numericType,numericType)))
  }
}