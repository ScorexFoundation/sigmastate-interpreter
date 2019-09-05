package sigmastate.verification.SigmaDsl.api

import sigmastate.verification.SigmaDsl.api.collection.Coll
import sigmastate.verification.SigmaDsl.api.sigma.AvlTree
import stainless.annotation.{extern, ignore, library, pure}

import scala.reflect.ClassTag

@library
trait RType[A] {
  @ignore
  def classTag: ClassTag[A]

  @ignore
  def name: String = this.toString

  /** Returns true is data size of `x: A` is the same for all `x`.
    * This is useful optimizations of calculating sizes of collections. */
  @library
  def isConstantSize: Boolean
}

@library
object RType {
  @extern @pure
  implicit def collRType[A](implicit cT: RType[A]): RType[Coll[A]] = ???

  @extern @pure
  implicit def pairRType[A, B](implicit tA: RType[A], tB: RType[B]): RType[(A, B)] = ???

  @extern @pure
  implicit def ByteType: RType[Byte] = ???

  @extern @pure
  implicit def IntType: RType[Int] = ???

  @extern @pure
  implicit def LongType: RType[Long] = ???

  @extern @pure
  implicit def AvlTreeRType: RType[AvlTree] = ???
}

