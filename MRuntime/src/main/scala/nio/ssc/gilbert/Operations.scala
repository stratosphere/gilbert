/**
 * gilbert - Distributed Linear Algebra on Sparse Matrices
 * Copyright (C) 2013  Sebastian Schelter
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package nio.ssc.gilbert

object ScalarOperation extends Enumeration {
  type ScalarOperation = Value
  val Binarize = Value
  val Negate = Value
}

object ScalarsOperation extends Enumeration {
  type ScalarsOperation = Value
  //TODO does norm2 belong here?
  val Addition, Subtraction, Multiplication, Division, Maximum, Norm2, Exponentiation = Value
}

object UnaryScalarOperation extends Enumeration{
  type UnaryScalarOperation = Value
  val UnaryMinus = Value
}

object CellwiseOperation extends Enumeration {
  type CellwiseOperation = Value
  val Addition, Subtraction, Multiplication, Division = Value
}

object VectorwiseOperation extends Enumeration {
  type VectorwiseOperation = Value
  val Max, Min, Average, Norm2Squared, Norm2, NormalizeL1 = Value
}

object MatrixwiseOperation extends Enumeration {
  type MatrixwiseOperation = Value
  val RowSums = Value
}