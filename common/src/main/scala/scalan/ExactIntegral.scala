package scalan

import scalan.util.Extensions._

import scala.math.Numeric.{ByteIsIntegral, LongIsIntegral, ShortIsIntegral, IntIsIntegral}

/** Integral operations with overflow checks.
  * Raise exception when overflow is detected.
  * Each instance of this typeclass overrides three methods `plus`, `minus`, `times`.
  * All other methods are implemented by delegating to the corresponding Integral instance from
  * standard Scala library.
  * This trait is used in core IR to avoid implicitly using standard scala implementations.
  */
trait ExactIntegral[T] {
  val n: Integral[T]
  def zero = fromInt(0)
  def one = fromInt(1)
  def plus(x: T, y: T): T = n.plus(x, y)
  def minus(x: T, y: T): T = n.minus(x, y)
  def times(x: T, y: T): T = n.times(x, y)
  def quot(x: T, y: T): T = n.quot(x, y)
  def divisionRemainder(x: T, y: T): T = n.rem(x, y)
  def negate(x: T): T = n.negate(x)
  def fromInt(x: Int): T = n.fromInt(x)
  def toInt(x: T): Int = n.toInt(x)
  def toLong(x: T): Long = n.toLong(x)
  def toFloat(x: T): Float = n.toFloat(x)
  def toDouble(x: T): Double = n.toDouble(x)
  def compare(x: T, y: T): Int = n.compare(x, y)
}

/** ExactIntegral instances for all types. */
object ExactIntegral {

  implicit object ByteIsExactIntegral extends ExactIntegral[Byte] {
    val n = scala.math.Numeric.ByteIsIntegral
    override def plus(x: Byte, y: Byte): Byte = x.addExact(y)
    override def minus(x: Byte, y: Byte): Byte = x.subtractExact(y)
    override def times(x: Byte, y: Byte): Byte = x.multiplyExact(y)
  }

  implicit object ShortIsExactIntegral extends ExactIntegral[Short] {
    val n = scala.math.Numeric.ShortIsIntegral
    override def plus(x: Short, y: Short): Short = x.addExact(y)
    override def minus(x: Short, y: Short): Short = x.subtractExact(y)
    override def times(x: Short, y: Short): Short = x.multiplyExact(y)
  }

  implicit object IntIsExactIntegral extends ExactIntegral[Int] {
    val n = scala.math.Numeric.IntIsIntegral
    override def plus(x: Int, y: Int): Int = java7.compat.Math.addExact(x, y)
    override def minus(x: Int, y: Int): Int = java7.compat.Math.subtractExact(x, y)
    override def times(x: Int, y: Int): Int = java7.compat.Math.multiplyExact(x, y)
  }

  implicit object LongIsExactIntegral extends ExactIntegral[Long] {
    val n = scala.math.Numeric.LongIsIntegral
    override def plus(x: Long, y: Long): Long = java7.compat.Math.addExact(x, y)
    override def minus(x: Long, y: Long): Long = java7.compat.Math.subtractExact(x, y)
    override def times(x: Long, y: Long): Long = java7.compat.Math.multiplyExact(x, y)
  }
}
