package sigmastate.verified

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

  /** Descriptor used to represent primitive types. */
  @ignore
  case class PrimitiveType[A](classTag: ClassTag[A]) extends RType[A] {
    override def name: String = classTag.toString()

    /** We assume all primitive types have inhabitants of the same size. */
    override def isConstantSize: Boolean = true

  }

  @ignore
  case class CollType[A](tA: RType[A]) extends RType[Coll[A]] {
    val classTag: ClassTag[Coll[A]] = {
      implicit val ctA: ClassTag[A] = tA.classTag
      scala.reflect.classTag[Coll[A]]
    }

    override def name: String = s"Coll[${tA.name}]"

    override def isConstantSize: Boolean = false
  }

  @ignore
  case class PairType[A, B](tFst: RType[A], tSnd: RType[B]) extends RType[(A, B)] {
    val classTag: ClassTag[(A, B)] = scala.reflect.classTag[(A, B)]

    override def name: String = s"(${tFst.name}, ${tSnd.name})"

    override def isConstantSize: Boolean = tFst.isConstantSize && tSnd.isConstantSize
  }

  @extern @pure
  implicit def collRType[A](implicit tA: RType[A]): RType[Coll[A]] = CollType(tA)

  @extern @pure
  implicit def pairRType[A, B](implicit tA: RType[A], tB: RType[B]): RType[(A, B)] = PairType(tA, tB)

  @extern @pure
  implicit def BooleanType: RType[Boolean] = PrimitiveType[Boolean](ClassTag.Boolean)

  @extern @pure
  implicit def ByteType: RType[Byte] = PrimitiveType[Byte](ClassTag.Byte)

  @extern @pure
  implicit def IntType: RType[Int] = PrimitiveType[Int](ClassTag.Int)

  @extern @pure
  implicit def LongType: RType[Long] = PrimitiveType[Long](ClassTag.Long)

  @extern @pure
  implicit def AvlTreeRType: RType[AvlTree] = ???
}

