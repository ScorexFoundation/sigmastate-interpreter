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
    require(sers.size == set.size, {
      val dupGroups = sers.groupBy { case (b, _) => b }.filter { case (_, g) => g.size > 1 }.toList
      s"expected distinct codes, got duplicated: $dupGroups"
    })
    val array = Array.fill[T](256)(null.asInstanceOf[T]) // one item for each OpCode
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

  /** Add new values to this container. */
  def add(code: Byte, value: T) = {
    val index = codeToIndex(code)
    require(sparseArray(index) == null, s"Value with index $index already defined.")
    sparseArray(index) = value
  }

  def remove(code: Byte) = {
    val index = codeToIndex(code)
    require(sparseArray(index) != null,
      s"Value with index $index not-defined, probably incorrect attempt to remove it.")
    sparseArray(index) = null.asInstanceOf[T]
  }
}

object SparseArrayContainer {

  def buildForSerializers(sers: Seq[ValueSerializer[_ <: Value[SType]]]): SparseArrayContainer[ValueSerializer[_ <: Value[SType]]] = {
    new SparseArrayContainer(sers.map(s => (s.opCode, s)))
  }
}
