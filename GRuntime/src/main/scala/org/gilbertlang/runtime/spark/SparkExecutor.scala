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

package org.gilbertlang.runtime.spark

import scala.collection.JavaConversions._
import org.apache.mahout.math.{SequentialAccessSparseVector, DenseVector, RandomAccessSparseVector, Vector}
import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.rdd.RDD
import org.gilbertlang.runtime.VectorFunctions
import org.gilbertlang._
import org.apache.mahout.math.function.Functions
import org.apache.mahout.math.random.Normal
import optimization.CommonSubexpressionDetector
import shell.{printPlan, withSpark}
import org.apache.spark.serializer.KryoSerializer
import GilbertFunctions._
import org.gilbertlang.error.ExecutionRuntimeError
import org.gilbertlang.runtime.CellwiseFunctions
import org.gilbertlang.shell.PlanPrinter
import org.gilbertlang.VoidExecutable

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

        handle[LoadMatrix, (String, Int, Int)](transformation,
            { x => (evaluate[String](x.path), evaluate[Double](x.numRows).toInt, evaluate[Double](x.numColumns).toInt)},
            { case (transformation, (path, rows, cols)) => {
              sc.textFile(path, degreeOfParallelism).map({ line => {
                val fields = line.split(" ")
                (fields(0).toInt, fields(1).toInt, fields(2).toDouble)
              }}).groupBy(_._1).flatMap({ case (index, elements) => {
                val vector = new RandomAccessSparseVector(cols)
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

      case IterationStatePlaceholder => { iterationState }
      
      case transformation: CellwiseMatrixMatrixTransformation => {
        handle[CellwiseMatrixMatrixTransformation, (RowPartitionedMatrix, RowPartitionedMatrix)](transformation,
            { transformation => {
              (evaluate[RowPartitionedMatrix](transformation.left), 
                  evaluate[RowPartitionedMatrix](transformation.right))
            } },
            { case (transformation, (a,b)) => {
              transformation.operation match {
                case Addition => a zip b map {
                  case((aIdx, aRow), (bIdx, bRow)) => (aIdx, aRow.plus(bRow))
                }
                case Multiplication => a zip b map {
                  case((aIdx, aRow), (bIdx, bRow)) => (aIdx, aRow.times(bRow))
                }
                case Division => a zip b map {
                  case((aIdx, aRow), (bIdx, bRow)) => (aIdx, aRow.assign(bRow, Functions.DIV))
                }
                case Subtraction => a zip b map {
                  case ((aIdx, aRow), (bIdx, bRow)) => (aIdx, aRow.minus(bRow))
                }
                case Maximum => {
                  a zip b map {
                    case ((aIdx, aRow), (bIdx, bRow)) => (aIdx, aRow.assign(bRow, Functions.MAX))
                  }
                }
                case Minimum => {
                  a zip b map {
                    case ((aIdx, aRow), (bIdx, bRow)) => (aIdx, aRow.assign(bRow, Functions.MIN))
                  }
                }
              }
            }})
      }


      case (transformation: CellwiseMatrixTransformation) => {

        handle[CellwiseMatrixTransformation, RowPartitionedMatrix](transformation,
            { transformation => evaluate[RowPartitionedMatrix](transformation.matrix) },
            { (transformation, matrix) => {
              transformation.operation match {
                case Binarize => {
                  //TODO add binarize to mahout to only apply it to non zeros!
                  matrix.map({ case (index, row) => (index, row.assign(VectorFunctions.binarize)) })
                }
                case Minus => {
                  matrix.map({ case (index,row) => (index, row.times(-1))})
                }
              }
            }})
      }
      
      case transformation: VectorwiseMatrixTransformation => {
        handle[VectorwiseMatrixTransformation, RowPartitionedMatrix](transformation,
            {transformation => evaluate[RowPartitionedMatrix](transformation.matrix)},
            {(transformation, matrix) => {
              transformation.operation match {
                case Maximum => {
                  matrix.map( { case (index, row) => (index, new DenseVector(Array(row.maxValue()))) })
                }
                case Minimum => {
                  matrix.map( {case (index, row) => (index, new DenseVector(Array(row.minValue())))})
                }
                case Norm2 => {
                  matrix.map( {case (index, row) => (index, new DenseVector(Array(row.norm(2))))})
                }
                case NormalizeL1 => {
                  matrix.map( {case (index, row) => (index, row.normalize(1))})
                }
              }
            }
             })
      }

      case (transformation: AggregateMatrixTransformation) => {

        handle[AggregateMatrixTransformation, RowPartitionedMatrix](transformation,
            { transformation => evaluate[RowPartitionedMatrix](transformation.matrix) },
            { (transformation, matrix) => {
              transformation.operation match {
                case Maximum => {
                  matrix.map({ case (_, row) => row.maxValue() }).aggregate(Double.MinValue)(math.max, math.max)
                }
                case Minimum => {
                  matrix.map({ case (_, row) => row.minValue()} ).aggregate(Double.MaxValue)(math.min, math.min)
                }
                case Norm2 => {
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

        handle[ScalarMatrixTransformation, (Double, RowPartitionedMatrix)](transformation,
          { transformation => {
            (evaluate[Double](transformation.scalar), evaluate[RowPartitionedMatrix](transformation.matrix))
          }},
          { case (transformation, (value, matrix)) => {
            transformation.operation match {
              case (Division) => {
                matrix.map({ case (index, row) => {
                  val newVector = new DenseVector(row.size()).assign(value)
                  (index, newVector.assign(row, Functions.DIV))
                  }
                })
              }
              case Addition => {
                matrix.map({ case(index, row) => {
                  (index, row.plus(value)) } })
              }
              case Multiplication => {
                matrix.map({ case(index, row) => {
                  (index, row.times(value)) } })
              }
              case Subtraction => {
                matrix.map({ case(index, row) => {
                	(index, row.assign(Functions.NEGATE).plus(value))
                }
                })
              }
            }
          }})
      }
            
      case transformation: MatrixScalarTransformation => {
        handle[MatrixScalarTransformation, (RowPartitionedMatrix, Double)](transformation, 
            {transformation => {
              (evaluate[RowPartitionedMatrix](transformation.matrix), evaluate[Double](transformation.scalar))
            }},
            { case (transformation, (matrix, scalar)) => {
              transformation.operation match{
                case Addition => {
                  matrix map {
                    case (index, row) => (index, row.plus(scalar))
                  }
                }
                case Subtraction => {
                  matrix map {
                    case (index, row) => (index, row.assign(Functions.MINUS,scalar))
                  }
                }
                case Multiplication => {
                  matrix map {
                    case (index, row) => (index, row.times(scalar))
                  }
                }
                case Division => {
                  matrix map {
                    case (index, row) => (index, row.divide(scalar))
                  }
                }
              }
            }})
      }
      
      case transformation: ScalarScalarTransformation => {
        handle[ScalarScalarTransformation, (Double, Double)](transformation,
            {transformation => {
              (evaluate[Double](transformation.left), evaluate[Double](transformation.right))
            }},
            {case (transformation, (a,b)) => {
              transformation.operation match {
                case Addition => a+b
                case Subtraction => a-b
                case Multiplication => a*b
                case Division => a/b
                case Maximum => math.max(a,b)
                case Minimum => math.min(a,b)
              }
            }})
      }
      
      case transformation: UnaryScalarTransformation => {
        handle[UnaryScalarTransformation, Double](transformation,
            {transformation => evaluate[Double](transformation.scalar)},
            { (transformation, scalar) => {
            	transformation.operation match {
            	  case Binarize => CellwiseFunctions.binarize(scalar)
            	  case Minus => -scalar
            	}
            }})
      }
     

      //TODO rework
      case (transformation: ones) => {

        handle[ones, (Int,Int)](transformation,
            { x =>  (evaluate[Double](x.numRows).toInt, evaluate[Double](x.numColumns).toInt)},
            { case (transformation, (numRows, numColumns)) => {
              var rows = Seq[(Int, Vector)]()

              for (index <- 1 to numRows) {
                rows = rows ++ Seq((index, new DenseVector(numColumns).assign(1)))
              }
              sc.parallelize(rows, degreeOfParallelism)
            }})
      }

      //TODO rework
      case (transformation: rand) => {

        handle[rand, (Int,Int,Double,Double)](transformation,
            { x => (evaluate[Double](x.numRows).toInt, evaluate[Double](x.numColumns).toInt, evaluate[Double](x.mean),
                evaluate[Double](x.std))},
            { case (transformation, (numRows, numColumns, mean, std)) => {

              val gaussian = new Normal(mean, std)
              var rows = Seq[(Int, Vector)]()

              for (index <- 1 to numRows) {
                rows = rows ++ Seq((index, new DenseVector(numColumns).assign(gaussian)))
              }
              sc.parallelize(rows, degreeOfParallelism)
            }})
      }
      
      case transformation: spones => {
        handle[spones, RowPartitionedMatrix](transformation,
            {transformation => evaluate[RowPartitionedMatrix](transformation.matrix)},
            {(_,matrix) => {
              matrix map { case (index, row) => {
                (index, row.assign(CellwiseFunctions.binarize))
              }}
            }})
      }
      
      case transformation: sumRow => {
        handle[sumRow, RowPartitionedMatrix](transformation,
            { transformation => evaluate[RowPartitionedMatrix](transformation.matrix)},
            { (_, matrix) => {
              matrix map {
                case (index, row) => 
                  val newRow = new RandomAccessSparseVector(1)
                  newRow.setQuick(0,row.zSum())
                  (index,  newRow)
              }
            }})
      }
      
      case transformation: sumCol => {
        handle[sumCol, RowPartitionedMatrix](transformation,
            {transforamtion => evaluate[RowPartitionedMatrix](transformation.matrix)},
            { (_, matrix) => {
              matrix.reduce({ case ((_,a),(_,b)) => {
                (0,a.plus(b))
              }})
            }})
      }
      
      case transformation: sum => {
        handle[sum, (RowPartitionedMatrix, Int)](transformation,
            { transformation => (evaluate[RowPartitionedMatrix](transformation.matrix),
                evaluate[Double](transformation.dimension).toInt)},
            { case (_, (matrix, 2)) => {
              matrix map {
                case (index, row) => 
                  val newRow = new RandomAccessSparseVector(1)
                  newRow.setQuick(0,row.zSum())
                  (index,  newRow)
              }
            }
            case (_, (matrix, 1)) => {
              matrix.reduce({ case ((_,a),(_,b)) => {
                (0,a.plus(b))
              }})
              }
            })
      }
      
      case transformation: diag => {
        handle[diag, RowPartitionedMatrix](transformation,
            { transformation => evaluate[RowPartitionedMatrix](transformation.matrix)},
            { (transformation, matrix) => {
            	(transformation.rows, transformation.cols) match{
            	  case (None,_) | (_, None) => {
            	    val numRows = matrix.count
            	    matrix flatMap { case(index, row) =>{
            	      if(row.size() ==1){
            	        val newRow = new RandomAccessSparseVector(numRows.toInt,1)
            	        newRow.setQuick(index,row.get(0))
            	        Some((index,newRow))
            	      }
            	      else{
            	        val maxIdx = math.min(numRows,row.size)
            	        if(index < maxIdx){
            	          val newRow = new RandomAccessSparseVector(1,1)
            	          newRow.setQuick(0,row.get(index))
            	          Some((index, newRow))
            	        }else{
            	          None
            	        }
            	      } 
            	    }}
            	  }
            	  
            	  case (Some(1), Some(y)) => {
            	    require(matrix.count == 1)
            	    matrix flatMap { case (index, row) => {
            	      for(elem <- row.nonZeroes())
            	        yield({
            	          val newRow = new RandomAccessSparseVector(y,1)
            	          newRow.setQuick(elem.index(),elem.get())
            	          (elem.index(),newRow)
            	        })
            	    }}
            	  }
            	  
            	  case (Some(x), Some(1)) => {
            	    matrix map { case (index, row) => {
            	      require(row.size() == 1)
            	      val newRow = new RandomAccessSparseVector(x,1)
            	      newRow.setQuick(index, row.get(0))
            	      (index, newRow)
            	    }}
            	  }
            	  
            	  case (Some(x), Some(y)) => {
            	    val maxIdx = math.min(x,y)
            	    matrix map { case (index, row) => {
            	      val newRow = new RandomAccessSparseVector(1)
            	      newRow.setQuick(0,row.get(math.min(index,maxIdx)))
            	      (index, newRow)
            	    }} take maxIdx
            	  }
            	}
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
      
      case transformation: WriteString => {
        handle[WriteString, Unit](transformation,
        		{_ => },
        		{(transformation, _) => println(transformation.string)})
      }
      
      case transformation: WriteFunction => {
        handle[WriteFunction, Unit](transformation,
            { _ => },
            {(transformation, _) => PlanPrinter.print(transformation.function) })
      }

      case (transformation: scalar) => {

        handle[scalar, Unit](transformation,
            { _ => },
            { (transformation, _) => transformation.value })
      }
      
      
      case transformation: string => {
        handle[string, Unit](transformation,
            { _ => },
            {(transformation, _) => transformation.value })
      }
      
      case transformation: function => {
        throw new ExecutionRuntimeError("Functions cannot be executed")
      }
      
      case transformation: Parameter => {
        throw new ExecutionRuntimeError("Parameters cannot be executed")
      }

      case (transformation: WriteScalarRef) => {

        handle[WriteScalarRef, Double](transformation,
            { transformation => evaluate[Double](transformation.scalar) },
            { (transformation, value) => println(value) })
      }
      
      case transformation: CompoundExecutable => {
       transformation.executables foreach { execute(_) }
      }
      
      case VoidExecutable =>{
        ()
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
