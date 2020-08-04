package sigmastate.eval

import java.math.BigInteger

import scalan.{ExactNumeric, ExactIntegral, ExactOrderingImpl}

import scala.math.{Integral, Ordering}
import special.sigma._
import sigmastate.eval.Extensions._
import special.collection.Coll

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

  trait GroupElementOrdering extends Ordering[GroupElement] {
    /** Compares `x: ECPoint` string representation with `y: ECPoint` string for order.
      * @returns a negative integer, zero, or a positive integer as the
      * `x` is less than, equal to, or greater than `y`.
      */
    def compare(x: GroupElement, y: GroupElement) = {
      SigmaDsl.toECPoint(x).toString.compareTo(SigmaDsl.toECPoint(y).toString)
    }
  }
  implicit object GroupElementOrdering extends GroupElementOrdering

  trait AvlTreeOrdering extends Ordering[AvlTree] {
    /** Compares this `x: AvlTree` string representation with `y: AvlTree` string for order.
      * @returns a negative integer, zero, or a positive integer as the
      * `x` is less than, equal to, or greater than `y`.
      */
    def compare(x: AvlTree, y: AvlTree) = {
      x.toString.compareTo(y.toString)
    }
  }
  implicit object AvlTreeOrdering extends AvlTreeOrdering

  class CollOrdering[T: Ordering] extends Ordering[Coll[T]] {
    implicit val O = implicitly[Ordering[Iterable[T]]]

    /** Compares this `x: Coll` with `y: Coll` using Ordering for underlying Array.
      * @returns a negative integer, zero, or a positive integer as the
      * `x` is less than, equal to, or greater than `y`.
      */
    def compare(x: Coll[T], y: Coll[T]) = {
      O.compare(x.toArray, y.toArray)
    }
  }
  implicit def collOrdering[T: Ordering]: Ordering[Coll[T]] = new CollOrdering[T]

  trait BoxOrdering extends Ordering[Box] {
    /** Compares this `x: Box` string representation with `y: Box` string for order.
      * @returns a negative integer, zero, or a positive integer as the
      * `x` is less than, equal to, or greater than `y`.
      */
    def compare(x: Box, y: Box) = {
      x.toString.compareTo(y.toString)
    }
  }
  implicit object BoxOrdering extends BoxOrdering

  trait PreHeaderOrdering extends Ordering[PreHeader] {
    /** Compares this `x: PreHeader` with `y: PreHeader` using block height.
      * @returns a negative integer, zero, or a positive integer as the
      * `x` is less than, equal to, or greater than `y`.
      */
    def compare(x: PreHeader, y: PreHeader) = {
      Ordering.Int.compare(x.height, y.height)
    }
  }
  implicit object PreHeaderOrdering extends PreHeaderOrdering

  trait HeaderOrdering extends Ordering[Header] {
    /** Compares this `x: Header` with `y: Header` using block height.
      * @returns a negative integer, zero, or a positive integer as the
      * `x` is less than, equal to, or greater than `y`.
      */
    def compare(x: Header, y: Header) = {
      Ordering.Int.compare(x.height, y.height)
    }
  }
  implicit object HeaderOrdering extends HeaderOrdering

  trait ContextOrdering extends Ordering[Context] {
    val O = Ordering[(Int, Coll[Byte])]

    /** Compares this `x: Context` with `y: Context` using block height and SELF.id.
      * @returns a negative integer, zero, or a positive integer as the
      * `x` is less than, equal to, or greater than `y`.
      */
    def compare(x: Context, y: Context) = {
      O.compare((x.HEIGHT, x.SELF.id), (y.HEIGHT, y.SELF.id))
    }
  }
  implicit object ContextOrdering extends ContextOrdering

  trait SigmaPropOrdering extends Ordering[SigmaProp] {
    /** Compares this `x: SigmaProp` with `y: SigmaProp` using string representation.
      * @returns a negative integer, zero, or a positive integer as the
      * `x` is less than, equal to, or greater than `y`.
      */
    def compare(x: SigmaProp, y: SigmaProp) = {
      x.toString.compareTo(y.toString)
    }
  }
  implicit object SigmaPropOrdering extends SigmaPropOrdering
}

object NumericOps {

  trait BigIntegerIsIntegral extends Integral[BigInteger] {
//    private val BI = implicitly[Integral[BigInt]]
    def quot(x: BigInteger, y: BigInteger): BigInteger = x.divide(y)
    def rem(x: BigInteger, y: BigInteger): BigInteger = x.remainder(y)
    def plus(x: BigInteger, y: BigInteger): BigInteger = x.add(y)
    def minus(x: BigInteger, y: BigInteger): BigInteger = x.subtract(y)
    def times(x: BigInteger, y: BigInteger): BigInteger = x.multiply(y)
    def negate(x: BigInteger): BigInteger = x.negate()
    def fromInt(x: Int): BigInteger = BigInteger.valueOf(x)
    def toInt(x: BigInteger): Int = x.intValueExact()
    def toLong(x: BigInteger): Long = x.longValueExact()
    def toFloat(x: BigInteger): Float = x.floatValue()
    def toDouble(x: BigInteger): Double = x.doubleValue()
  }
  implicit object BigIntegerIsIntegral extends BigIntegerIsIntegral with OrderingOps.BigIntegerOrdering

  trait BigIntIsIntegral extends Integral[BigInt] {
    def quot(x: BigInt, y: BigInt): BigInt = x.divide(y)
    def rem(x: BigInt, y: BigInt): BigInt = x.remainder(y)
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
  implicit object BigIntIsIntegral extends BigIntIsIntegral with OrderingOps.BigIntOrdering

  implicit object BigIntIsExactNumeric extends ExactNumeric[BigInt] {
    val n = BigIntIsIntegral
    override def plus(x: BigInt, y: BigInt): BigInt = n.plus(x, y)
    override def minus(x: BigInt, y: BigInt): BigInt = n.minus(x, y)
    override def times(x: BigInt, y: BigInt): BigInt = n.times(x, y)
  }

  implicit object BigIntIsExactIntegral extends ExactIntegral[BigInt] {
    val n = BigIntIsIntegral
    override def plus(x: BigInt, y: BigInt): BigInt = n.plus(x, y)
    override def minus(x: BigInt, y: BigInt): BigInt = n.minus(x, y)
    override def times(x: BigInt, y: BigInt): BigInt = n.times(x, y)
  }

  implicit object BigIntIsExactOrdering extends ExactOrderingImpl[BigInt](BigIntIsIntegral)
}

