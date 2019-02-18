package sigmastate.eval

import java.math.BigInteger

import scala.math.{LowPriorityOrderingImplicits,Integral, Ordering}
import special.sigma._
import sigma.util.Extensions._

object OrderingOps extends LowPriorityOrderingImplicits {
  def apply[T](implicit ord: Ordering[T]) = ord

  trait BigIntegerOrdering extends Ordering[BigInteger] {
    def compare(x: BigInteger, y: BigInteger) = x.compareTo(y)
  }
  implicit object BigInteger extends BigIntegerOrdering

  trait BigIntOrdering extends Ordering[BigInt] {
    def compare(x: BigInt, y: BigInt) = x.compareTo(y)
  }
  implicit object BigInt extends BigIntOrdering
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
//    private val BI = implicitly[Integral[BigInt]]
    def quot(x: BigInt, y: BigInt): BigInt = x.divide(y)
    def rem(x: BigInt, y: BigInt): BigInt = x.remainder(y)
    def plus(x: BigInt, y: BigInt): BigInt = x.add(y)
    def minus(x: BigInt, y: BigInt): BigInt = x.subtract(y)
    def times(x: BigInt, y: BigInt): BigInt = x.multiply(y)
    def negate(x: BigInt): BigInt = x.negate()
    def fromInt(x: Int): BigInt = CostingSigmaDslBuilder.BigInt(x.toBigInt)
    def toInt(x: BigInt): Int = x.toInt
    def toLong(x: BigInt): Long = x.toLong
    def toFloat(x: BigInt): Float = CostingSigmaDslBuilder.toBigInteger(x).floatValue()
    def toDouble(x: BigInt): Double = CostingSigmaDslBuilder.toBigInteger(x).doubleValue()
  }
  implicit object BigIntIsIntegral extends BigIntIsIntegral with OrderingOps.BigIntOrdering

}

