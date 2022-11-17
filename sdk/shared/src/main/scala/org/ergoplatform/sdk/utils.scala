package org.ergoplatform.sdk

import scala.collection.mutable
import scala.reflect.ClassTag

object utils {
  def outerJoin[K, L, R, O]
      (left: Map[K, L], right: Map[K, R])
          (l: (K, L) => O, r: (K, R) => O, inner: (K, L, R) => O): Map[K, O] = {
    val res = mutable.HashMap.empty[K, O]
    val lks = left.keySet
    val rks = right.keySet
    val leftOnly = lks diff rks
    val rightOnly = rks diff lks
    val both = lks intersect rks
    for ( lk <- leftOnly )
      res += lk -> l(lk, left(lk))
    for ( rk <- rightOnly )
      res += rk -> r(rk, right(rk))
    for ( k <- both )
      res += k -> inner(k, left(k), right(k))
    res.toMap
  }

  def mapToArrays[K: ClassTag, V: ClassTag](m: Map[K, V]): (Array[K], Array[V]) = {
    val keys = mutable.ArrayBuilder.make[K]
    val values = mutable.ArrayBuilder.make[V]
    for ( (k, v) <- m ) {
      keys += k
      values += v
    }
    (keys.result, values.result)
  }
}
