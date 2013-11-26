package de.tuberlin.dima.stratosphere.gilbert.mtyper.types

import MTypes.Helper._
import MTypes._
import MValues.Helper._
import de.tuberlin.dima.stratosphere.gilbert.mtyper.types.MTypedAst._


object ConvenienceMethods {
  def newTVV() = {
    (newTV(),newVV(),newVV())
  }
  
  def newNTVV() = {
    (newNumericTV(),newVV(),newVV())
  }
  
  def getType(stmt : TypedStatement): MType = {
    stmt match{
      case x:TypedExpression => x.datatype
      case TypedOutputResultStatement(innerStmt) => getType(innerStmt)
      case TypedAssignment(lhs,rhs) => rhs.datatype
      case TypedNOP => VoidType
    }
  }

}