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

sealed trait CellwiseOperation
sealed trait ScalarsOperation extends CellwiseOperation
sealed trait ScalarMatrixOperation extends CellwiseOperation

sealed trait BasicArithmeticOperation extends ScalarsOperation with ScalarMatrixOperation
case object Addition extends BasicArithmeticOperation
case object Subtraction extends BasicArithmeticOperation
case object Multiplication extends BasicArithmeticOperation
case object Division extends BasicArithmeticOperation

sealed trait MinMax extends ScalarsOperation with VectorwiseOperation with AggregateMatrixOperation
case object Maximum extends MinMax
case object Minimum extends MinMax

sealed trait UnaryScalarOperation
case object Minus extends UnaryScalarOperation
case object Binarize extends UnaryScalarOperation

sealed trait VectorwiseOperation
case object NormalizeL1 extends VectorwiseOperation

sealed trait AggregateMatrixOperation

sealed trait NormOperation extends AggregateMatrixOperation with VectorwiseOperation 
case object Norm2 extends NormOperation