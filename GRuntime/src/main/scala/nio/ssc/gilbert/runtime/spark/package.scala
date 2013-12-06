package nio.ssc.gilbert.runtime

import org.apache.mahout.math.{Matrix, MatrixWritable, VectorWritable, Vector}

package object spark {

  implicit def v2Writable(v: Vector): VectorWritable = new VectorWritable(v)
  implicit def m2Writable(m: Matrix): MatrixWritable = new MatrixWritable(m)
  implicit def vw2v(vw: VectorWritable): Vector = vw.get()
  implicit def mw2m(mw: MatrixWritable): Matrix = mw.get()
}
