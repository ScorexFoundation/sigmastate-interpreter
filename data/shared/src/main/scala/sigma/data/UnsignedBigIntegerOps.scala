package sigma.data

import sigma._
import sigma.data.UnsignedBigIntOrderingOps.UnsignedBigIntOrdering
import sigma.eval.Extensions.IntExt

import scala.math.{Integral, Ordering}

object UnsignedBigIntOrderingOps {
  def apply[T](implicit ord: Ordering[T]) = ord

  trait UnsignedBigIntOrdering extends Ordering[UnsignedBigInt] {
    def compare(x: UnsignedBigInt, y: UnsignedBigInt) = x.compareTo(y)
  }
  implicit object UnsignedBigIntOrdering extends UnsignedBigIntOrdering
}

object UnsignedBigIntNumericOps {

  /** Base implementation of Integral methods for UnsignedBigInt. */
  trait UnsignedBigIntIsIntegral extends Integral[UnsignedBigInt] {
    /** This method should not be used in v4.x */
    def quot(x: UnsignedBigInt, y: UnsignedBigInt): UnsignedBigInt = x.divide(y)

    /** This method is used in ErgoTreeEvaluator based interpreter, to implement
      * '%' operation of ErgoTree (i.e. `%: (T, T) => T` operation) for all
      * numeric types T including BigInt.
      *
      * In the v4.x interpreter, however, the `%` operation is implemented using
      * [[CBigInt]].mod method , which delegates to [[java.math.BigInteger]].mod method.
      *
      * Even though this method is called `rem`, the semantics of ErgoTree
      * language requires it to correspond to [[java.math.BigInteger]].mod
      * method.
      *
      * For this reason we define implementation of this `rem` method using
      * [[BigInt]].mod.
      *
      * NOTE: This method should not be used in v4.x
      */
    def rem(x: UnsignedBigInt, y: UnsignedBigInt): UnsignedBigInt = x.mod(y)

    def plus(x: UnsignedBigInt, y: UnsignedBigInt): UnsignedBigInt = x.add(y)
    def minus(x: UnsignedBigInt, y: UnsignedBigInt): UnsignedBigInt = x.subtract(y)
    def times(x: UnsignedBigInt, y: UnsignedBigInt): UnsignedBigInt = x.multiply(y)
    def negate(x: UnsignedBigInt): UnsignedBigInt = ???
    def fromInt(x: Int): UnsignedBigInt = x.toUnsignedBigInt
    def toInt(x: UnsignedBigInt): Int = x.toInt
    def toLong(x: UnsignedBigInt): Long = x.toLong
    def toFloat(x: UnsignedBigInt): Float = ???
    def toDouble(x: UnsignedBigInt): Double = ???
  }

  /** The instance of Integral for BigInt.
    *
    * Note: ExactIntegral was not defined for [[sigma.BigInt]] in v4.x.
    * This is because arithmetic BigInt operations were handled in a special way
    * (see `case op: ArithOp[t] if op.tpe == SBigInt =>` in RuntimeCosting.scala).
    * As result [[scalan.primitives.UnBinOps.ApplyBinOp]] nodes were not created for
    * BigInt operations in v4.x., and hence operation descriptors such as
    * [[scalan.primitives.NumericOps.IntegralDivide]] and
    * [[scalan.primitives.NumericOps.IntegralMod]] were not used for BigInt.
    * NOTE: this instance is used in the new v5.0 interpreter.
    */
  object UnsignedBigIntIsIntegral extends UnsignedBigIntIsIntegral with UnsignedBigIntOrdering {
    def parseString(str: String): Option[UnsignedBigInt] = ???
  }

  /** The instance of [[ExactIntegral]] typeclass for [[BigInt]]. */
  implicit object UnsignedBigIntIsExactIntegral extends ExactIntegral[UnsignedBigInt] {
    val n = UnsignedBigIntIsIntegral
    override def plus(x: UnsignedBigInt, y: UnsignedBigInt): UnsignedBigInt = n.plus(x, y)
    override def minus(x: UnsignedBigInt, y: UnsignedBigInt): UnsignedBigInt = n.minus(x, y)
    override def times(x: UnsignedBigInt, y: UnsignedBigInt): UnsignedBigInt = n.times(x, y)

    override def quot(x: UnsignedBigInt, y: UnsignedBigInt): UnsignedBigInt = x.divide(y)

    /** This method is used in ErgoTreeEvaluator based interpreter, to implement
      * '%' operation of ErgoTree (i.e. `%: (T, T) => T` operation) for all
      * numeric types T including BigInt.
      *
      * In the v4.x interpreter, however, the `%` operation is implemented using
      * [[CBigInt]].mod method, which delegates to [[java.math.BigInteger]].mod method.
      *
      * Even though this method is called `divisionRemainder`, the semantics of ErgoTree
      * language requires it to correspond to [[java.math.BigInteger]].mod method.
      *
      * For this reason we define implementation of this method using [[BigInt]].mod.
      *
      * NOTE: This method should not be used in v4.x
      */
    override def divisionRemainder(x: UnsignedBigInt, y: UnsignedBigInt): UnsignedBigInt = x.mod(y)
  }

  /** The instance of [[scalan.ExactOrdering]] typeclass for [[BigInt]]. */
  implicit object UnsignedBigIntIsExactOrdering extends ExactOrderingImpl[UnsignedBigInt](UnsignedBigIntIsIntegral)
}

