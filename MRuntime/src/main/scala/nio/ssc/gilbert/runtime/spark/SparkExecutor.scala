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

package nio.ssc.gilbert.runtime.spark

import scala.collection.JavaConversions._

import org.apache.mahout.math.{SequentialAccessSparseVector, DenseVector, RandomAccessSparseVector, Vector}

import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._

import org.apache.spark.rdd.RDD
import nio.ssc.gilbert.runtime.VectorFunctions
import nio.ssc.gilbert._

import org.apache.mahout.math.function.Functions
import org.apache.mahout.math.random.Normal
import optimization.CommonSubexpressionDetector
import shell.{printPlan, withSpark}
import org.apache.spark.serializer.KryoSerializer

object SparkExecutorRunner {

  //def eigen(A: Matrix) = fixpoint(rand(3), { b => (A * b) / norm2(A * b) })

  def main(args: Array[String]): Unit = {
    val A = load("/home/ssc/Desktop/gilbert/test/matrix.tsv", 3, 3)

    val b = ones(3, 1) / math.sqrt(3)

    withSpark(norm2(A * b))
  }
}

class SparkExecutor extends Executor {

  type RowPartitionedMatrix = RDD[(Int, Vector)]

  System.setProperty("spark.serializer", classOf[KryoSerializer].getName)
  System.setProperty("spark.kryo.registrator", classOf[MahoutKryoRegistrator].getName)

  val sc = new SparkContext("local", "Gilbert")
  val degreeOfParallelism = 2

  def run(executable: Executable) = {

    setRedirects(new CommonSubexpressionDetector().find(executable))

    printPlan(executable)

    execute(executable)
  }

  var iterationState: Vector = null

  protected def execute(executable: Executable): Any = {

    executable match {

      case (transformation: LoadMatrix) => {

        handle[LoadMatrix, Unit](transformation,
            { _ => },
            { (transformation, _) => {
              sc.textFile(transformation.path, degreeOfParallelism).map({ line => {
                val fields = line.split(" ")
                (fields(0).toInt, fields(1).toInt, fields(2).toDouble)
              }}).groupBy(_._1).flatMap({ case (index, elements) => {
                val vector = new RandomAccessSparseVector(transformation.numColumns)
                for ((_, column, value) <- elements) {
                  vector.setQuick(column, value)
                }
                Seq((index, new SequentialAccessSparseVector(vector)))
              }})
            }})
      }

      case (transformation: FixpointIteration) => {

        iterationState = handle[FixpointIteration, Vector](transformation,
        { transformation => evaluate[Vector](transformation.initialState) },
        { (_, initialVector) => initialVector }).asInstanceOf[Vector]

        for (_ <- 1 to 10) {
          iterationState = handle[FixpointIteration, Vector](transformation,
          { transformation => evaluate[Vector](transformation.updatePlan) },
          { (_, vector) => vector }).asInstanceOf[Vector]
        }

        iterationState
      }

      case (transformation: IterationStatePlaceholder) => { iterationState }


      case (transformation: CellwiseMatrixTransformation) => {

        handle[CellwiseMatrixTransformation, RowPartitionedMatrix](transformation,
            { transformation => evaluate[RowPartitionedMatrix](transformation.matrix) },
            { (transformation, matrix) => {
              transformation.operation match {
                case ScalarOperation.Binarize => {
                  //TODO add binarize to mahout to only apply it to non zeros!
                  matrix.map({ case (index, row) => (index, row.assign(VectorFunctions.binarize)) })
                }
              }
            }})
      }

      case (transformation: AggregateMatrixTransformation) => {

        handle[AggregateMatrixTransformation, RowPartitionedMatrix](transformation,
            { transformation => evaluate[RowPartitionedMatrix](transformation.matrix) },
            { (transformation, matrix) => {
              transformation.operation match {
                case ScalarsOperation.Maximum => {
                  matrix.map({ case (_, row) => row.maxValue() }).aggregate(Double.MinValue)(math.max, math.max)
                }
                case ScalarsOperation.Norm2 => {
                  val sumOfEntriesSquared =
                    matrix.map({ case (_, row) => row.getLengthSquared() }).aggregate(0.0)({ _ + _ }, { _ + _ })
                  math.sqrt(sumOfEntriesSquared)
                }
              }
            }})
      }

      case (transformation: Transpose) => {

        handle[Transpose, RowPartitionedMatrix](transformation,
            { transformation => evaluate[RowPartitionedMatrix](transformation.matrix)},
            { (transformation, matrix) => {
              //TODO make reduce combinable
              matrix.flatMap({ case (index, row) => {
                for (elem <- row.nonZeroes())
                  yield { (elem.index(), index, elem.get()) }
              }})
                .groupBy(_._1).map({ case (index, entries) => {
                val row = new RandomAccessSparseVector(Integer.MAX_VALUE)
                entries.foreach({ case (_, columnIndex, value) => row.setQuick(columnIndex, value) })
                (index, new SequentialAccessSparseVector(row))
              }})
            }})
      }

       //TODO we should eliminate transpose before
      case (transformation: MatrixMult) => {

        handle[MatrixMult, (RowPartitionedMatrix, RowPartitionedMatrix)](transformation,
            { transformation => {
              val leftMatrix = evaluate[RowPartitionedMatrix](transformation.left)
              val rightMatrix = evaluate[RowPartitionedMatrix](transformation.right)
              (leftMatrix, rightMatrix)
            }},
            { case (_, (leftMatrix, rightMatrix)) => {

              //TODO make reduce combinable
              /* row outer product formulation of matrix multiplication */
              val transposedLeftMatrix = leftMatrix.flatMap({ case (index, row) => {
                  for (elem <- row.nonZeroes())
                    yield { (elem.index(), index, elem.get()) }
                }})
                .groupBy(_._1).map({ case (index, entries) => {
                  val row = new RandomAccessSparseVector(Integer.MAX_VALUE)
                  entries.foreach({ case (_, columnIndex, value) => row.setQuick(columnIndex, value) })
                  (index, new SequentialAccessSparseVector(row))
                }})

              transposedLeftMatrix.join(rightMatrix).flatMap({ case (_, (column, row)) => {
                for (elem <- column.nonZeroes())
                  yield { (elem.index(), row.times(elem.get())) }
              }})
              .reduceByKey(_.assign(_, Functions.PLUS))

            }})
      }

      case (transformation: ScalarMatrixTransformation) => {

        handle[ScalarMatrixTransformation, (RowPartitionedMatrix, Double)](transformation,
          { transformation => {
            (evaluate[RowPartitionedMatrix](transformation.matrix), evaluate[Double](transformation.scalar))
          }},
          { case (transformation, (matrix, value)) => {
            transformation.operation match {
              case (ScalarsOperation.Division) => {
                matrix.map({ case (index, row) => (index, row.assign(Functions.DIV, value)) })
              }
            }
          }})
      }

      //TODO rework
      case (transformation: ones) => {

        handle[ones, Unit](transformation,
            { _ => },
            { (transformation, _) => {
              var rows = Seq[(Int, Vector)]()

              for (index <- 1 to transformation.rows) {
                rows = rows ++ Seq((index, new DenseVector(transformation.columns).assign(1)))
              }
              sc.parallelize(rows, degreeOfParallelism)
            }})
      }

      //TODO rework
      case (transformation: rand) => {

        handle[rand, Unit](transformation,
            { _ => },
            { (transformation, _) => {

              val gaussian = new Normal(transformation.mean, transformation.std)
              var rows = Seq[(Int, Vector)]()

              for (index <- 1 to transformation.rows) {
                rows = rows ++ Seq((index, new DenseVector(transformation.columns).assign(gaussian)))
              }
              sc.parallelize(rows, degreeOfParallelism)
            }})
      }

      case (transformation: WriteMatrix) => {

        handle[WriteMatrix, RowPartitionedMatrix](transformation,
            { transformation => evaluate[RowPartitionedMatrix](transformation.matrix) },
            { (_, matrix) => {
              matrix.foreach({ case (index, row) =>
                println(index + " " + row)
              })
            }})
      }

      case (transformation: scalar) => {

        handle[scalar, Unit](transformation,
            { _ => },
            { (transformation, _) => transformation.value })
      }

      case (transformation: WriteScalarRef) => {

        handle[WriteScalarRef, Double](transformation,
            { transformation => evaluate[Double](transformation.scalar) },
            { (transformation, value) => println(value) })
      }
    }

  }

  def toVector(values: RDD[(Int,Double)]) = {

    //TODO fixme
    val vector = new RandomAccessSparseVector(3)
    for ((row, sum) <- values.toArray()) {
      vector.setQuick(row, sum)
    }
    new SequentialAccessSparseVector(vector)
  }

}
