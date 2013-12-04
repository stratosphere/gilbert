package org.gilbertlang.mlibrary

import org.gilbertlang.mlibrary.MTypes._
import org.gilbertlang.mlibrary.MTypes.Helper._
import org.gilbertlang.mlibrary.MValues._
import org.gilbertlang.mlibrary.MValues.Helper._
import ConvenienceMethods._

object MBuiltinSymbols extends BuiltinSymbols {
  val binarize = Symbol("binarize", binarizeType)
  val load = Symbol("load", loadType)
  val maxValue = Symbol("maxValue", maxValueType)
  val spones = Symbol("spones", sponesType)
  val sum = Symbol("sum",sumType)
  val diag = Symbol("diag",diagType)
  val ones = Symbol("ones",onesType)
  val fixpoint = Symbol("fixpoint", fixpointType)
  val write = Symbol("write",writeType)

  def loadType = FunctionType((MatrixType(CharacterType, IntValue(1), newVV()), IntegerType, IntegerType),
      MatrixType(DoubleType, ReferenceValue(1), ReferenceValue(2)))

  def binarizeType = {
    val (t, a, b) = newNTVV()
    val numericType = newNumericTV()
    PolymorphicType(List(
      FunctionType(MatrixType(t, a, b), MatrixType(IntegerType, a, b)),
      FunctionType(numericType, IntegerType)))
  }

  def maxValueType = {
    val (t, a, b) = newNTVV()
    val numericType = newNumericTV()
    PolymorphicType(List(
      FunctionType(MatrixType(t, a, b), t),
      FunctionType(numericType, numericType)))
  }
  
  def sponesType = {
    val (t,a,b) = newNTVV()
    FunctionType(MatrixType(t,a,b), MatrixType(t,a,b))
  }
  
  def sumType = {
    val (t,a,b) = newTVV()
    val (c,d) = (newVV(),newVV())
    PolymorphicType(List(
        FunctionType((MatrixType(t,a,b),IntegerType),MatrixType(t,c,d))))
  }
  
  def diagType = {
    val t = newNumericTV()
    val a = newVV()
    val t1 = newNumericTV()
    val a1 = newVV()
    val t2 = newNumericTV()
    val a2 = newVV()
    PolymorphicType(List(
        FunctionType(MatrixType(t,a,IntValue(1)),MatrixType(t,a,a)),
        FunctionType(MatrixType(t1,IntValue(1),a1),MatrixType(t1,a1,a1)),
        FunctionType(MatrixType(t2,a2,a2),MatrixType(t2,a2,IntValue(1)))))
  }
  
  def onesType = {
    PolymorphicType(List(
        FunctionType(IntegerType,MatrixType(IntegerType,ReferenceValue(1),ReferenceValue(1))),
        FunctionType((IntegerType, IntegerType), MatrixType(IntegerType,ReferenceValue(1), ReferenceValue(2)))))
  }
  
  def fixpointType = {
    val t = newTV()
    FunctionType((t,FunctionType(t,t)), t)
  }
  
  def writeType = {
    val (t,a,b) = newTVV()
    PolymorphicType(List(
        FunctionType((MatrixType(t,a,b),MatrixType(CharacterType,IntValue(1),newVV())),VoidType)))
  }

}