package nio.ssc.gilbert.runtime.stratosphere

//import io.ssc.gilbert.{WriteMatrix, LoadMatrix, Executable}
//import eu.stratosphere.scala.{ScalaPlan, DataStream, TextFile}
//import eu.stratosphere.scala.operators.DelimitedDataSinkFormat
//import eu.stratosphere.pact.client.LocalExecutor


//object StratosphereExecutor {
//
//  def main(args: Array[String]): Unit = {
//    val A = LoadMatrix("/home/ssc/Desktop/gilbert/test/matrix.tsv");
//    execute(WriteMatrix(A))
//  }
//
//  def execute(executable: Executable): Any = {
//
//    executable match {
//      case (op: LoadMatrix) => {
//
//       TextFile(op.path).map({ line => {
//         val fields = line.split(" ")
//         (fields(0).toInt, fields(1).toInt, fields(2).toDouble)
//       }}).groupBy(_._1).reduceGroup({ entries => {
//
//         var index = -1
//
//         val vector = new Array[Double](3)
//          entries.foreach({ case (row, column, value) => {
//            index = row
//            vector(column) = value
//          }})
//          (index, vector)
//       }})
//      }
//
//      case (op:WriteMatrix) => {
//        val matrix = execute(op.in).asInstanceOf[DataStream[(Int, Array[Double])]]
//        val sink = matrix.write("/tmp/strato/", DelimitedDataSinkFormat(format))
//
//        val plan = new ScalaPlan(Seq(sink), "Gilbert")
//        plan.setDefaultParallelism(2)
//
//        LocalExecutor.execute(plan)
//      }
//    }
//  }
//
//  def format = (row: (Int, Array[Double])) =>  { row._1 + " " + row._2 }
//
//}
