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

package nio.ssc.gilbert.shell

import nio.ssc.gilbert._

object printPlan {

  def main(args: Array[String]) = {
    printPlan(Examples.cooccurrences)
  }

  def apply(executable: Executable) = {
    new PlanPrinter().print(executable)
  }
}

class PlanPrinter {

  def print(executable: Executable, depth: Int = 0): Unit = {

    executable match {

      case (op: LoadMatrix) => {
        printIndented(depth, op,"LoadMatrix [" + op.path + "]")
      }

      case (op: FixpointIteration) => {
        printIndented(depth, op,"FixpointIteration")
        print(op.initialState, depth + 1)
        print(op.updatePlan, depth + 1)
      }

      case (op: IterationStatePlaceholder) => {
        printIndented(depth, op,"IterationState")
      }

      case (op: CellwiseMatrixTransformation) => {
        printIndented(depth, op,"CellwiseMatrixOp [" + op.operation + "]")
        print(op.matrix, depth + 1)
      }

      case (op: CellwiseMatrixMatrixTransformation) => {
        printIndented(depth, op, "CellwiseMatrixMatrixTransformation [" + op.operation + "]")
        print(op.left, depth + 1)
        print(op.right, depth + 1)
      }

      case (op: Transpose) => {
        printIndented(depth, op,"Transpose")
        print(op.matrix, depth + 1)
      }

      case (op: MatrixMult) => {
        printIndented(depth, op,"MatrixMult")
        print(op.left, depth + 1)
        print(op.right, depth + 1)
      }

      case (op: AggregateMatrixTransformation) => {
        printIndented(depth, op,"AggregateMatrixOp [" + op.operation + "]")
        print(op.matrix, depth + 1)
      }

      case (op: VectorwiseMatrixTransformation) => {
        printIndented(depth, op,"VectorwiseMatrixTransformation [" + op.operation + "]")
        print(op.matrix, depth + 1)
      }

      case (op: ScalarMatrixTransformation) => {
        printIndented(depth, op,"ScalarMatrixOp [" + op.operation + "]")
        print(op.matrix, depth + 1)
        print(op.scalar, depth + 1)
      }

      case (op: ones) => {
        printIndented(depth, op,op.toString)
      }

      case (op: rand) => {
        printIndented(depth, op,op.toString)
      }

      case (op: WriteMatrix) => {
        printIndented(depth, op,"WriteMatrix")
        print(op.matrix, depth + 1)
      }

      case (op: scalar) => {
        printIndented(depth, op,op.value.toString)
      }

      case (op: WriteScalarRef) => {
        printIndented(depth, op,"WriteScalarRef")
        print(op.scalar, depth + 1)
      }
      
      case op: CompoundExecutable =>
        printIndented(depth, op, "CompoundExecutable(" )
        op.executables foreach { print(_,depth+1)}
    }

  }

  def printIndented(depth: Int, transformation: Executable, str: String) = {
    println("".padTo(depth, "  ").mkString + "(" + transformation.id + ") " + str)
  }
}
