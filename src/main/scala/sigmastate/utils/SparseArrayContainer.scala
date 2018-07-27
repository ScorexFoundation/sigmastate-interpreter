package sigmastate.utils

import sigmastate.SType
import sigmastate.Values.Value
import sigmastate.serialization.ValueSerializer

import scala.reflect.ClassTag

/**
  * Store values in a sparse array (null if no value for a code)
  * @param values - list of pairs (code, value)
  */
class SparseArrayContainer[T: ClassTag](values: Seq[(Byte, T)]) {

  private val sparseArray: Array[T] = build(values)

  private def build(sers: Seq[(Byte, T)]): Array[T] = {
    val set = sers.map(_._1).toSet
    require(sers.size == set.size,
      s"expected distinct codes, got duplicated: ${sers.groupBy { case (b, _) => b }.filter { case (k, g) => g.size > 1 }.toList }")
    val array = Array.fill[T](256)(null.asInstanceOf[T])
    sers.foreach { case (code, value) =>
        array(codeToIndex(code)) = value
    }
    array
  }

  @inline
  private def codeToIndex(code: Byte): Int = code + 128

  /**
    * Returns value for the given code
    * @param code of a value
    * @return value or null if no value for a given code
    */
  @inline
  def get(code: Byte): T = sparseArray(codeToIndex(code))

}

object SparseArrayContainer {

  def buildFrom(sers: Seq[ValueSerializer[_ <: Value[SType]]]): SparseArrayContainer[ValueSerializer[_ <: Value[SType]]] = {
    new SparseArrayContainer(sers.map(s => (s.opCode, s)))
  }
}
