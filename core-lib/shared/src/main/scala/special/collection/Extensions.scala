package special.collection

import debox.cfor

object Extensions {
  /** Extension methods for `Coll[T]`. */
  implicit class CollOps[T](val source: Coll[T]) extends AnyVal {
    /** Applies a function `f` to each element of the `source` collection. */
    def foreach(f: T => Unit) = {
      val limit = source.length
      cfor(0)(_ < limit, _ + 1) { i =>
        f(source(i))
      }
    }
  }

  /** Extension methods for `Coll[(A,B)]`.
    * Has priority over the extensions in [[CollOps]].
    */
  implicit class PairCollOps[A,B](val source: Coll[(A,B)]) extends AnyVal {
    /** Applies a function `f` to each pair of elements in the `source` collection. */
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
