package sigmastate.eval

import java.math.BigInteger

import scalan.{ExactNumeric, ExactOrderingImpl, ExactIntegral}

import scala.math.{Integral, Ordering}
import special.sigma._
import sigmastate.eval.Extensions._

object OrderingOps {
  def apply[T](implicit ord: Ordering[T]) = ord

  trait BigIntegerOrdering extends Ordering[BigInteger] {
    def compare(x: BigInteger, y: BigInteger) = x.compareTo(y)
  }
  implicit object BigIntegerOrdering extends BigIntegerOrdering

  trait BigIntOrdering extends Ordering[BigInt] {
    def compare(x: BigInt, y: BigInt) = x.compareTo(y)
  }
  implicit object BigIntOrdering extends BigIntOrdering
}

object NumericOps {

  /** Base implementation of Integral methods for BigInt. */
  trait BigIntIsIntegral extends Integral[BigInt] {
    def quot(x: BigInt, y: BigInt): BigInt = ??? // this method should not be used in v4.x
    def rem(x: BigInt, y: BigInt): BigInt = ??? // this method should not be used in v4.x
    def plus(x: BigInt, y: BigInt): BigInt = x.add(y)
    def minus(x: BigInt, y: BigInt): BigInt = x.subtract(y)
    def times(x: BigInt, y: BigInt): BigInt = x.multiply(y)
    def negate(x: BigInt): BigInt = x.negate()
    def fromInt(x: Int): BigInt = x.toBigInt
    def toInt(x: BigInt): Int = x.toInt
    def toLong(x: BigInt): Long = x.toLong
    def toFloat(x: BigInt): Float = CostingSigmaDslBuilder.toBigInteger(x).floatValue()
    def toDouble(x: BigInt): Double = CostingSigmaDslBuilder.toBigInteger(x).doubleValue()
  }

  /** The instance of Integral for BigInt.
    *
    * Note: ExactIntegral is not defined for [[special.sigma.BigInt]].
    * This is because arithmetic BigInt operations are handled specially
    * (see `case op: ArithOp[t] if op.tpe == SBigInt =>` in RuntimeCosting.scala).
    * As result [[scalan.primitives.UnBinOps.ApplyBinOp]] nodes are not created for BigInt
    * operations, and hence operation descriptors such as
    * [[scalan.primitives.NumericOps.IntegralDivide]] and
    * [[scalan.primitives.NumericOps.IntegralMod]] are not used for BigInt.
    */
  implicit object BigIntIsIntegral extends BigIntIsIntegral with OrderingOps.BigIntOrdering

  /** The instance of [[ExactIntegral]] typeclass for [[BigInt]]. */
  implicit object BigIntIsExactIntegral extends ExactIntegral[BigInt] {
    val n = BigIntIsIntegral
    override def plus(x: BigInt, y: BigInt): BigInt = n.plus(x, y)
    override def minus(x: BigInt, y: BigInt): BigInt = n.minus(x, y)
    override def times(x: BigInt, y: BigInt): BigInt = n.times(x, y)

    override def quot(x: BigInt, y: BigInt): BigInt =
      ??? // this method should not be used in v4.x

    override def divisionRemainder(x: BigInt, y: BigInt): BigInt =
      ??? // this method should not be used in v4.x
  }

  implicit val BigIntIsExactNumeric: ExactNumeric[BigInt] = BigIntIsExactIntegral

  /** The instance of [[scalan.ExactOrdering]] typeclass for [[BigInt]]. */
  implicit object BigIntIsExactOrdering extends ExactOrderingImpl[BigInt](BigIntIsIntegral)
}

