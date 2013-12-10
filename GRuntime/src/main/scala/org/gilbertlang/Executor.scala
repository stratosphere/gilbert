package org.gilbertlang


trait Executor {

  private var symbolTable = Map[Int, Any]()
  private var repeatedExpressions = Map[Int, Int]()
  private var volatileExpressions = Set[Int]()

  def setRedirects(repeatedExpressions: Map[Int, Int]) = {
    this.repeatedExpressions = repeatedExpressions
  }
  
  def setVolatileExpressions(expressions: Set[Int]) {
    volatileExpressions = expressions
  }

  def run(executable: Executable): Any

  protected def execute(transformation: Executable): Any

  def evaluate[T](in: Executable) = {
    execute(in).asInstanceOf[T]
  }

  def handle[T <: Executable, I](executable: T, retrieveInput: (T) => I, handle: (T, I) => Any): Any = {


    if (!volatileExpressions.contains(executable.id)) {
      /* check if we already processed this expression */
      if (symbolTable.contains(executable.id)) {
        println("\t reusing (" + executable.id + ")")
        return symbolTable(executable.id)
      }

      /* check if this a common subexpression which we already processed */
      if (repeatedExpressions.contains(executable.id)) {
        println("\t reusing (" + executable.id + ") repeated subexpression ("+repeatedExpressions(executable.id)+")" )
        return symbolTable(repeatedExpressions(executable.id))
      }
    }
    
    val input = retrieveInput(executable)
    println("\t executing (" + executable.id + ") " + executable)

    val output = handle(executable, input)

    symbolTable += (executable.id -> output)

    output
  }
}
