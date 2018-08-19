package sigmastate.eval

import java.math.BigInteger

import scala.math.{LowPriorityOrderingImplicits, BigInt, Integral, Ordering}
import scala.math.Numeric.BigIntIsIntegral

object OrderingOps extends LowPriorityOrderingImplicits {
  def apply[T](implicit ord: Ordering[T]) = ord
  trait BigIntegerOrdering extends Ordering[BigInteger] {
    def compare(x: BigInteger, y: BigInteger) = x.compareTo(y)
  }
  implicit object BigInteger extends BigIntegerOrdering
}

object NumericOps {

  trait BigIntegerIsIntegral extends Integral[BigInteger] {
    private val BI = implicitly[Integral[BigInt]]
    def quot(x: BigInteger, y: BigInteger): BigInteger = x.divide(y)
    def rem(x: BigInteger, y: BigInteger): BigInteger = x.remainder(y)
    def plus(x: BigInteger, y: BigInteger): BigInteger = x.add(y)
    def minus(x: BigInteger, y: BigInteger): BigInteger = x.subtract(y)
    def times(x: BigInteger, y: BigInteger): BigInteger = x.multiply(y)
    def negate(x: BigInteger): BigInteger = x.negate()
    def fromInt(x: Int): BigInteger = BigInteger.valueOf(x)
    def toInt(x: BigInteger): Int = x.toInt()
    def toLong(x: BigInteger): Long = x.toLong()
    def toFloat(x: BigInteger): Float = x.toFloat()
    def toDouble(x: BigInteger): Double = x.toDouble()
  }
  implicit object BigIntegerIsIntegral extends BigIntegerIsIntegral with OrderingOps.BigIntegerOrdering

}

