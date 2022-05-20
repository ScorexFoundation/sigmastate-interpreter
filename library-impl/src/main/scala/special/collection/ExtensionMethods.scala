package special.collection

import scalan.RType

object ExtensionMethods {

  implicit class PairCollOps[A,B](val source: Coll[(A,B)]) extends AnyVal {
    import RType._
    @inline implicit def tA = source.tItem.tFst
    @inline implicit def tB = source.tItem.tSnd

    // TODO optimize
    def unionSetByKey(that: Coll[(A,B)]): Coll[(A,B)] = {
      source.unionSetByKey(that)
    }

    def reduceByKey(r: ((B,B)) => B): Coll[(A,B)] = {
      source.mapReduce(identity, r)
    }

    def sumByKey(implicit m: Monoid[B]): Coll[(A,B)] =
      reduceByKey(r => m.plus(r._1, r._2))

    def groupByKey: Coll[(A, Coll[B])] = source.groupByProjecting(_._1, _._2)

    @inline def mapFirst[A1: RType](f: A => A1): Coll[(A1, B)] = source.asInstanceOf[PairColl[A,B]].mapFirst(f)
    @inline def mapSecond[B1: RType](f: B => B1): Coll[(A, B1)] = source.asInstanceOf[PairColl[A,B]].mapSecond(f)
  }

  implicit class NestedCollOps[A](val source: Coll[Coll[A]]) extends AnyVal {
    @inline implicit def tA = source.tItem.tItem

    def flatten: Coll[A] = source.builder.flattenColl(source)
  }

}
