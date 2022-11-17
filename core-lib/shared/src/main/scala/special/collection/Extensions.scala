package special.collection

import debox.cfor
import scalan.RType

object Extensions {
  implicit class CollOps[T](val coll: Coll[T]) extends AnyVal {
    def foreach(f: T => Unit) = {
      val limit = coll.length
      cfor(0)(_ < limit, _ + 1) { i =>
        f(coll(i))
      }
    }
  }

  implicit class PairCollOps[A,B](val source: Coll[(A,B)]) extends AnyVal {
    @inline def mapFirst[A1: RType](f: A => A1): Coll[(A1, B)] = source.asInstanceOf[PairColl[A,B]].mapFirst(f)
    @inline def mapSecond[B1: RType](f: B => B1): Coll[(A, B1)] = source.asInstanceOf[PairColl[A,B]].mapSecond(f)

    def foreach(f: (A, B) => Unit) = {
      val b = source.builder
      val (as, bs) = b.unzip(source)
      val limit = source.length
      cfor(0)(_ < limit, _ + 1) { i =>
        f(as(i), bs(i))
      }
    }
  }

}
