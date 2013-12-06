package org.gilbertlang.runtime.spark

import com.esotericsoftware.kryo.{Kryo, Serializer}
import com.esotericsoftware.kryo.io.{Input, Output}
import org.apache.hadoop.io.{DataInputBuffer, DataOutputBuffer, Writable}
import org.apache.spark.serializer.KryoRegistrator
import org.apache.mahout.math.{MatrixWritable, VectorWritable, Matrix, DenseVector, Vector}

/**
 *
 * @author dmitriy
 */
class MahoutKryoRegistrator extends KryoRegistrator {

  override def registerClasses(kryo: Kryo) = {

    kryo.addDefaultSerializer(classOf[Vector], new WritableKryoSerializer[Vector, VectorWritable])
    kryo.addDefaultSerializer(classOf[DenseVector], new WritableKryoSerializer[Vector, VectorWritable])
    kryo.addDefaultSerializer(classOf[Matrix], new WritableKryoSerializer[Matrix, MatrixWritable])

  }
}

class WritableKryoSerializer[V <% Writable, W <: Writable <% V : ClassManifest] extends Serializer[V] {

  def write(kryo: Kryo, out: Output, v: V) = {
    val dob = new DataOutputBuffer()
    v.write(dob)
    dob.close()

    out.writeInt(dob.getLength)
    out.write(dob.getData, 0, dob.getLength)
  }

  def read(kryo: Kryo, in: Input, vClazz: Class[V]): V = {
    val dib = new DataInputBuffer()
    val len = in.readInt()
    val data = new Array[Byte](len)
    in.read(data)
    dib.reset(data, len)
    val w: W = classManifest[W].erasure.newInstance().asInstanceOf[W]
    w.readFields(dib)
    w
  }
}