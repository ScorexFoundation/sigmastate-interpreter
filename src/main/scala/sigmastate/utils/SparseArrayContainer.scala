package sigmastate.utils

import sigmastate.SType
import sigmastate.Values.Value
import sigmastate.serialization.ValueSerializer

import scala.collection.mutable
import scala.reflect.ClassTag

/**
  * Store values in a sparse array (null if no value for a code)
  * @param values - list of pairs (code, value)
  */
class SparseArrayContainer[T: ClassTag](values: Seq[(Byte, T)]) {

  private val sparseArray: Array[T] = build(values)

  private def build(sers: Seq[(Byte, T)]): Array[T] = {
    require(sers.size == sers.map(_._1).toSet.size, s"expected distinct codes, got: $sers")
    val b = mutable.ArrayBuilder.make[T]()
    val mappedSers: Map[Byte, T] = sers.toMap
    for (i <- Byte.MinValue to Byte.MaxValue) {
      mappedSers.get(i.toByte) match {
        case Some(v) => b += v
        case None => b += null.asInstanceOf[T]
      }
    }
    b.result()
  }

  /**
    * Returns value for the given code
    * @param code of a value
    * @return value or null if no value for a given code
    */
  @inline
  def get(code: Byte): T = sparseArray(code + 128)

}

object SparseArrayContainer {

  def buildFrom(sers: Seq[ValueSerializer[_ <: Value[SType]]]): SparseArrayContainer[ValueSerializer[_ <: Value[SType]]] = {
    new SparseArrayContainer(sers.map(s => (s.opCode, s)))
  }
}
