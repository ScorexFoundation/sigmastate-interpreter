package sigmastate.verification.SigmaDsl

import sigmastate.verification.SigmaDsl.api.collection.Coll
import stainless.annotation.{extern, library, pure}

@library
object Coll {

  @extern @pure
  def empty[A]: Coll[A] = ??? //new CollOverList(List())

  @extern @pure
  def apply[A](a: A*): Coll[A] = ??? // new CollOverList(List(a))

//  @library @extern
//  def apply[A](list: List[A]): Coll[A] = ??? // new CollOverList(list)

//  @library
//  implicit class ListOps[A](val list: List[A]) {
//    @extern
//    def toColl: Coll[A] = Coll(list)
//  }

//  @library
//  implicit class CollOps[A](val coll: Coll[A]) {
//    @extern
//    def toList: List[A] = ???
//  }
}
