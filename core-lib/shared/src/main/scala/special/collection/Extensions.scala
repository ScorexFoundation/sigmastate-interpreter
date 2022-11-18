package special.collection

import debox.cfor

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
