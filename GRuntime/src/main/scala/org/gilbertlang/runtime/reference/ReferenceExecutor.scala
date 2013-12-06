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

package org.gilbertlang.runtime.reference

import org.gilbertlang._
import org.apache.mahout.math.{DenseMatrix, SparseRowMatrix}
import runtime.VectorFunctions
import org.apache.mahout.math.random.Normal
import optimization.CommonSubexpressionDetector
import shell.{local, printPlan}
import scala.io.Source
import org.gilbertlang.EmptyExecutable

object ReferenceExecutorRunner {

  def main(args: Array[String]): Unit = {

    val A = load("/home/ssc/Desktop/gilbert/test/matrix.tsv", 3, 3)

    val b = ones(3, 1) / math.sqrt(3)

    local(norm2(A * b))
  }
}

class ReferenceExecutor extends Executor {

  type MahoutMatrix = org.apache.mahout.math.Matrix

  def run(executable: Executable) = {

    setRedirects(new CommonSubexpressionDetector().find(executable))

    printPlan(executable)

    execute(executable)
  }

  //TODO fix this
  var iterationState: MahoutMatrix = null

  protected def execute(executable: Executable): Any = {

    executable match {
      case (compound: CompoundExecutable) =>
        compound.executables foreach { execute(_) }
      case EmptyExecutable => ()
      case (transformation: LoadMatrix) => {

        handle[LoadMatrix, Unit](transformation,
            { _ => },
            { (transformation, _) => {

              val matrix = new SparseRowMatrix(transformation.numRows, transformation.numColumns)

              for (line <- Source.fromFile(transformation.path).getLines()) {
                val fields = line.split(" ")
                matrix.setQuick(fields(0).toInt - 1, fields(1).toInt - 1, fields(2).toDouble)
              }

              matrix
            }})
      }

      case (transformation: FixpointIteration) => {

        iterationState = handle[FixpointIteration, MahoutMatrix](transformation,
            { transformation => evaluate[MahoutMatrix](transformation.initialState) },
            { (_, initialVector) => initialVector }).asInstanceOf[MahoutMatrix]

        for (_ <- 1 to 10) {
          iterationState = handle[FixpointIteration, MahoutMatrix](transformation,
            { transformation => evaluate[MahoutMatrix](transformation.updatePlan) },
            { (_, vector) => vector }).asInstanceOf[MahoutMatrix]
        }

        iterationState
      }

      case (transformation: IterationStatePlaceholder) => { iterationState }

      case (transformation: CellwiseMatrixTransformation) => {

        handle[CellwiseMatrixTransformation, MahoutMatrix](transformation,
            { transformation => evaluate[MahoutMatrix](transformation.matrix) },
            { (transformation, matrix) => {
              transformation.operation match {
                case ScalarOperation.Binarize => { matrix.assign(VectorFunctions.binarize)}
                case ScalarOperation.Negate => {matrix.times(-1)}
              }
            }})
      }

      case (transformation: Transpose) => {

        handle[Transpose, MahoutMatrix](transformation,
            { transformation => evaluate[MahoutMatrix](transformation.matrix) },
            { (transformation, matrix) => matrix.transpose() })
      }
      
      case transformation:MatrixExponentiation => {
        handle[MatrixExponentiation,(MahoutMatrix, Integer)](transformation,
            { transformation => (evaluate[MahoutMatrix](transformation.matrix),evaluate[Integer](transformation.exponent)) },
            { case (_, (matrix, exponent)) => 
              var result = matrix
              (1 until exponent) foreach{ _ => result = result.times(matrix)}
              result})
      }

      case (transformation: MatrixMult) => {

        handle[MatrixMult, (MahoutMatrix, MahoutMatrix)](transformation,
            { transformation => {
              (evaluate[MahoutMatrix](transformation.left), evaluate[MahoutMatrix](transformation.right))
            }},
            { case (_, (leftMatrix, rightMatrix)) => leftMatrix.times(rightMatrix) })
      }

      case (transformation: AggregateMatrixTransformation) => {

        handle[AggregateMatrixTransformation, MahoutMatrix](transformation,
            { transformation => evaluate[MahoutMatrix](transformation.matrix) },
            { (transformation, matrix) => {
              transformation.operation match {
                case ScalarsOperation.Maximum => { matrix.aggregate(VectorFunctions.max, VectorFunctions.identity) }
                case ScalarsOperation.Norm2 => {
                  val sumOfSquaredEntries = matrix.aggregateRows(VectorFunctions.lengthSquared).zSum()
                  math.sqrt(sumOfSquaredEntries)
                }
              }
            }})
      }

      case (transformation: ScalarMatrixTransformation) => {

        handle[ScalarMatrixTransformation, (MahoutMatrix, Double)](transformation,
            { transformation => {
              (evaluate[MahoutMatrix](transformation.matrix), (evaluate[Double](transformation.scalar)))
            }},
            { case (transformation, (matrix, scalar)) => {
              transformation.operation match {
                case (ScalarsOperation.Division) => { matrix.divide(scalar) }
                case (ScalarsOperation.Multiplication) => { matrix.times(scalar) }
              }
            }})
      }

      case (transformation: VectorwiseMatrixTransformation) => {

        handle[VectorwiseMatrixTransformation, MahoutMatrix](transformation,
            { transformation => evaluate[MahoutMatrix](transformation.matrix) },
            { (transformation, matrix) => {
              transformation.operation match {
                case (VectorwiseOperation.NormalizeL1) => {
                  for (index <- 0 until matrix.numRows()) {
                    matrix.viewRow(index).normalize(1)
                  }
                }
                matrix
              }
            }})
      }

      case (transformation: ones) => {

        handle[ones, Unit](transformation,
            { _ => },
            { (transformation, _) => { new DenseMatrix(transformation.rows, transformation.columns).assign(1) }})
      }

      case (transformation: rand) => {

        handle[rand, Unit](transformation,
            { _ => },
            { (transformation, _) => {
              new DenseMatrix(transformation.rows, transformation.columns)
                 .assign(new Normal(transformation.mean, transformation.std))
            }})
      }

      case (transformation: WriteMatrix) => {

        handle[WriteMatrix, MahoutMatrix](transformation,
            { transformation => evaluate[MahoutMatrix](transformation.matrix) },
            { (_, matrix) => println(matrix) })
      }

      case (transformation: scalar) => {

        handle[scalar, Unit](transformation,
            { _ => },
            { (transformation, _) => transformation.value })
      }

      case (transformation: WriteScalarRef) => {

        handle[WriteScalarRef, Double](transformation,
            { transformation => evaluate[Double](transformation.scalar) },
            { (_, scalar) => println(scalar) })
      }
      
      case transformation: ScalarTransformation => 
        handle[ScalarTransformation,Double](transformation,
            { transformation => evaluate[Double](transformation.scalar) },
            { (transformation, value) => transformation.operation match{
              case UnaryScalarOperation.UnaryMinus => -value 
            }})
      case transformation: ScalarScalarTransformation =>
        handle[ScalarScalarTransformation,(Double,Double)](transformation,
            { transformation => (evaluate[Double](transformation.left),evaluate[Double](transformation.right)) },
            { case (transformation,(left,right)) => transformation.operation match{
              case ScalarsOperation.Addition => left + right
              case ScalarsOperation.Subtraction => left - right
              case ScalarsOperation.Division => left / right
              case ScalarsOperation.Multiplication => left * right
              case ScalarsOperation.Exponentiation => math.pow(left,right)
              case ScalarsOperation.Maximum => math.max(left,right)
              case ScalarsOperation.Norm2 => math.sqrt(left*left + right*right)
            }})
    }
    

  }
}

