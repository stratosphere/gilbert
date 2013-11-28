package nio.ssc.gilbert

import java.util.concurrent.atomic.AtomicInteger

object IDGenerator {

  val counter = new AtomicInteger(0)

  def nextID() = counter.incrementAndGet()
}


trait Executable {
  val id: Int = IDGenerator.nextID()
}


trait Executor {

  private var symbolTable = Map[Int, Any]()
  private var repeatedExpressions = Map[Int, Int]()

  def setRedirects(repeatedExpressions: Map[Int, Int]) = {
    this.repeatedExpressions = repeatedExpressions
  }

  def run(executable: Executable): Any

  protected def execute(transformation: Executable): Any

  def evaluate[T](in: Executable) = {
    execute(in).asInstanceOf[T]
  }

  def handle[T <: Executable, I](executable: T, retrieveInput: (T) => I, handle: (T, I) => Any): Any = {

    val input = retrieveInput(executable)


    if (executable.getClass != classOf[FixpointIteration]) {

      /* check if we already processed this expression */
      if (symbolTable.contains(executable.id)) {
        println("\t reusing (" + executable.id + ")")
        return symbolTable(executable.id)
      }

      /* check if this a common subexpression which we already processed */
      if (repeatedExpressions.contains(executable.id)) {
        println("\t reusing (" + executable.id + ") (repeated subexpression)")
        return symbolTable(repeatedExpressions(executable.id))
      }

    }

    println("\t executing (" + executable.id + ") " + executable)

    val output = handle(executable, input)

    symbolTable += (executable.id -> output)

    output
  }
}
