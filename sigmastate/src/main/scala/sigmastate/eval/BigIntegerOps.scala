package sigmastate.eval

import java.math.BigInteger

import scalan.{ExactNumeric, ExactIntegral, ExactOrderingImpl}

import scala.math.{LowPriorityOrderingImplicits, Integral, Ordering}
import special.sigma._
import scalan.util.Extensions._
import sigmastate.eval.Extensions._
import sigmastate.eval.NumericOps.BigIntIsExactNumeric.n

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

  trait BigIntIsIntegral extends Integral[BigInt] {
    def quot(x: BigInt, y: BigInt): BigInt = x.divide(y)
    def rem(x: BigInt, y: BigInt): BigInt = x.mod(y)
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

