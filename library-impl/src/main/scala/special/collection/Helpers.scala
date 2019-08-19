package special.collection

import scala.reflect.ClassTag
import scala.collection.mutable
import scalan.Internal
import spire.syntax.all._

object Helpers {
  private def sameLengthErrorMsg[A,B](xs: Coll[A], ys: Coll[B]) =
    s"Collections should have same length but was ${xs.length} and ${ys.length}:\n xs=$xs;\n ys=$ys"

  def assertSameLength[A,B](xs: Coll[A], ys: Coll[B]) = {
    assert(xs.length == ys.length, sameLengthErrorMsg(xs, ys))
  }

  def requireSameLength[A,B](xs: Coll[A], ys: Coll[B]) = {
    require(xs.length == ys.length, sameLengthErrorMsg(xs, ys))
  }

  @inline def asColl[T](coll: Coll[_]): Coll[T] = coll.asInstanceOf[Coll[T]]

  def mapReduce[A, K: ClassTag, V: ClassTag](arr: Array[A], m: A => (K, V), r: ((V, V)) => V): (Array[K], Array[V]) = {
    val keyPositions = new java.util.HashMap[K, Int](32)
    val keys = mutable.ArrayBuilder.make[K]
    val values = Array.ofDim[V](arr.length)
    var i = 0
    var nValues = 0
    while (i < arr.length) {
      val (key, value) = m(arr(i))
      val pos = keyPositions.getOrDefault(key, 0)
      if (pos == 0) {
        keyPositions.put(key, nValues + 1)
        keys += key
        values(nValues) = value
        nValues += 1
      } else {
        values(pos - 1) = r((values(pos - 1), value))
      }
      i += 1
    }
    val resValues = Array.ofDim[V](nValues)
    Array.copy(values, 0, resValues, 0, nValues)
    (keys.result(), resValues)
  }

  def mapToArrays[K: ClassTag, V: ClassTag](m: Map[K,V]): (Array[K], Array[V]) = {
    val keys = mutable.ArrayBuilder.make[K]
    val values = mutable.ArrayBuilder.make[V]
    for ((k,v) <- m) {
      keys += k
      values += v
    }
    (keys.result, values.result)
  }

}
