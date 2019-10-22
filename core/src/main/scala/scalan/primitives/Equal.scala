package scalan.primitives

import scalan.{Base, Scalan}

trait Equal extends Base { self: Scalan =>
  /** Binary operation representing structural equality between arguments. */
  case class Equals[A: Elem]() extends BinOp[A, Boolean]("==", equalValues[A](_, _))

  /** Binary operation representing structural inequality between arguments. */
  case class NotEquals[A: Elem]() extends BinOp[A, Boolean]("!=", !equalValues[A](_, _))

  protected def equalValues[A](x: Any, y: Any)(implicit eA: Elem[A]) = x == y

  /** Extension methods to construct ApplyBinOp nodes */
  implicit class EqualOps[A](x: Ref[A]) {
    implicit private val eA = x.elem

    /** Apply Equals binary operation and return Ref to ApplyBinOp node. */
    def ===(y: Ref[A]): Ref[Boolean] = Equals[A].apply(x, y)

    /** Apply NotEquals binary operation and return Ref to ApplyBinOp node. */
    def !==(y: Ref[A]): Ref[Boolean] = NotEquals[A].apply(x, y)
  }
}
