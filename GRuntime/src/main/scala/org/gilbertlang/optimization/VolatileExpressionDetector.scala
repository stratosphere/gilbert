package org.gilbertlang.optimization

import org.gilbertlang.Executable
import org.gilbertlang.FixpointIteration
import org.gilbertlang.IterationStatePlaceholder
import scala.collection.mutable.Stack

object VolatileExpressionDetector extends Walker {
  private var fixpointEnvironment = false
  private val volatileExpressions = scala.collection.mutable.Set[Int]()
  private val stack = Stack[Boolean]()

  def clear() = {
    fixpointEnvironment = false

  }
  def find(executable: Executable): Set[Int] = {
    clear()
    visit(executable)
    volatileExpressions.toSet
  }

  override def onArrival(transformation: Executable) {
    transformation match {
      case _: FixpointIteration => {
        fixpointEnvironment = true
        stack.push(false)
      }
      case IterationStatePlaceholder => {}
      case _ => stack.push(false)
    }
  }

  override def onLeave(transformation: Executable) {
    transformation match {
      case _: FixpointIteration => {
        fixpointEnvironment = false
        val volatile = stack.pop()
        if (volatile) {
          volatileExpressions.add(transformation.id)
        }
      }
      case IterationStatePlaceholder => {
        if (fixpointEnvironment) {
          volatileExpressions.add(IterationStatePlaceholder.id)
          stack.pop()
          stack.push(true)
        }
      }
      case _ => {
        val volatile = stack.pop()

        if (volatile) {
          volatileExpressions.add(transformation.id)
          stack.pop()
          stack.push(true)
        }
      }
    }
  }
}