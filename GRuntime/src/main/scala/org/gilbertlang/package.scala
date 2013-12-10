package org

package object gilbertlang {
  implicit def Int2Scalar(value: Int) = scalar(value.toDouble)
  implicit def Double2Scalar(value: Double) = scalar(value)
  implicit def String2StringRef(value: String) = string(value)
  
  implicit def MatrixMatrix2FunctionRef(fun : Matrix => Matrix): FunctionRef = {
    function(1,fun(MatrixParameter(0)))
  }
  
  def scalarRef2Int(value: ScalarRef) = {
    value match{
      case scalar(value) => Some(value.toInt)
      case _ => None
    }
  }
}
