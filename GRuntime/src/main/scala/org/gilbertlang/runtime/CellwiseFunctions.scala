package org.gilbertlang.runtime

import org.apache.mahout.math.function.DoubleDoubleFunction
import org.apache.mahout.math.function.DoubleFunction

object CellwiseFunctions {
	def divide = new DoubleDoubleFunction{
	  def apply(a: Double, b: Double) = { a/b}
	}
	
	def times = new DoubleDoubleFunction {
	  def apply(a: Double, b: Double) = a*b
	}
	
	def max = new DoubleDoubleFunction {
	  def apply(a: Double, b: Double) = math.max(a,b)
	}
	
	def min = new DoubleDoubleFunction {
	  def apply(a: Double, b: Double) = math.min(a,b)
	}
	
	def binarize = new DoubleFunction {
	  def apply(a: Double) = if(a ==0) 0 else 1
	}
}