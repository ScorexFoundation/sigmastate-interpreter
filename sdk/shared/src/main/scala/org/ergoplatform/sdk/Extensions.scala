package org.ergoplatform.sdk

import debox.cfor
import scalan.RType
import scalan.rtypeToClassTag
import special.collection.{Coll, CollBuilder, Helpers}

import scala.collection.immutable

object Extensions {

  implicit class CollOps[A](val coll: Coll[A]) extends AnyVal {

    /** Partitions this $coll in two $colls according to a predicate.
      *
      * @param pred the predicate on which to partition.
      * @return a pair of $colls: the first $coll consists of all elements that
      *         satisfy the predicate `p` and the second $coll consists of all elements
      *         that don't. The relative order of the elements in the resulting ${coll}s
      *         will BE preserved (this is different from Scala's version of this method).
      */
    def partition(pred: A => Boolean): (Coll[A], Coll[A]) = {
      val (ls, rs) = coll.toArray.partition(pred)
      val b = coll.builder
      implicit val tA: RType[A] = coll.tItem
      (b.fromArray(ls), b.fromArray(rs))
    }

    def toMap[T, U](implicit ev: A <:< (T, U)): immutable.Map[T, U] = {
      var b = immutable.Map.empty[T, U]
      val len = coll.length
      cfor(0)(_ < len, _ + 1) { i =>
        val kv = coll(i)
        if (b.contains(kv._1))
          throw new IllegalArgumentException(s"Cannot transform collection $this to Map: duplicate key in entry $kv")
        b = b + kv
      }
      b
    }
  }

  implicit class CollBuilderOps(val builder: CollBuilder) extends AnyVal {
    def outerJoin[K: RType, L, R, O: RType]
        (left: Coll[(K, L)], right: Coll[(K, R)])
            (
                l: ((K, L)) => O,
                r: ((K, R)) => O,
                inner: ((K, (L, R))) => O): Coll[(K, O)] = {
      val res = utils.outerJoin[K, L, R, O](left.toMap, right.toMap)(
        (k, lv) => l((k, lv)),
        (k, rv) => r((k, rv)),
        (k, lv, rv) => inner((k, (lv, rv))))
      fromMap(res)
    }

    def fromMap[K: RType, V: RType](m: Map[K, V]): Coll[(K, V)] = {
      val (ks, vs) = utils.mapToArrays(m)
      builder.pairCollFromArrays(ks, vs)
    }
  }
}
