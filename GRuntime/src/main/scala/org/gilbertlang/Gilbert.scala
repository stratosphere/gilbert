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

package org.gilbertlang

abstract class Matrix extends Executable {

  def transpose() = { Transpose(this) }

  def times(other: Matrix) = { MatrixMult(this, other) }
  def times(scalar: ScalarRef) = { ScalarMatrixTransformation(scalar, this, ScalarsOperation.Multiplication) }

  def div(scalar: ScalarRef) = { ScalarMatrixTransformation(scalar, this, ScalarsOperation.Division) }
  def plus(other: Matrix) = { CellwiseMatrixMatrixTransformation(this, other, CellwiseOperation.Addition) }
  
  def binarize() = { CellwiseMatrixTransformation(this, ScalarOperation.Binarize) }

  def max() = { AggregateMatrixTransformation(this, ScalarsOperation.Maximum) }
  def norm(p: Int) = {
    p match {
      case 2 => AggregateMatrixTransformation(this, ScalarsOperation.Norm2)
    }
  }

  def t() = transpose
  def *(other: Matrix) = times(other)
  def *(scalar: ScalarRef) = times(scalar)
  def +(other: Matrix) = plus(other)

  def /(scalar: ScalarRef) = div(scalar)

  def normalizeRows(norm: Int) = {
    norm match {
      case 1 => VectorwiseMatrixTransformation(this, VectorwiseOperation.NormalizeL1)
    }
  }
}

abstract class ScalarRef extends Executable {

  def times(matrix: Matrix) = { ScalarMatrixTransformation(this, matrix, ScalarsOperation.Multiplication) }
  def div(matrix: Matrix) = { ScalarMatrixTransformation(this, matrix, ScalarsOperation.Division) }
  def div(other: ScalarRef) = { ScalarScalarTransformation(this, other, ScalarsOperation.Division) }
  def minus(other: ScalarRef) = { ScalarScalarTransformation(this, other, ScalarsOperation.Subtraction) }

  def *(matrix: Matrix) = times(matrix)
  def -(other: ScalarRef) = minus(other)
}



