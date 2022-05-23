package special.collection

import scalan.RType

object ExtensionMethods {

  implicit class PairCollOps[A,B](val source: Coll[(A,B)]) extends AnyVal {
    @inline def mapFirst[A1: RType](f: A => A1): Coll[(A1, B)] = source.asInstanceOf[PairColl[A,B]].mapFirst(f)
    @inline def mapSecond[B1: RType](f: B => B1): Coll[(A, B1)] = source.asInstanceOf[PairColl[A,B]].mapSecond(f)
  }

}
