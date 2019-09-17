package scalan

import scalan.util.Extensions._

import scala.math.Numeric.{ByteIsIntegral, LongIsIntegral, ShortIsIntegral, IntIsIntegral}

/** Numeric operations with overflow checks.
  * Raise exception when overflow is detected.
  * Each instance of this typeclass overrides three methods `plus`, `minus`, `times`.
  * All other methods are implemented by delegating to the corresponding Numeric instance from
  * standard Scala library.
  * This trait is used in core IR to avoid implicitly using standard scala implementations
  */
trait ExactNumeric[T] {
  val n: Numeric[T]
  def plus(x: T, y: T): T
  def minus(x: T, y: T): T
  def times(x: T, y: T): T
  def negate(x: T): T = n.negate(x)
  def fromInt(x: Int): T = n.fromInt(x)
  def toInt(x: T): Int = n.toInt(x)
  def toLong(x: T): Long = n.toLong(x)
  def toFloat(x: T): Float = n.toFloat(x)
  def toDouble(x: T): Double = n.toDouble(x)
  def compare(x: T, y: T): Int = n.compare(x, y)
  def zero: T = n.zero
  def one: T = n.one
  def abs(x: T): T = n.abs(x)
}

/** ExactNumeric instances for all types. */
object ExactNumeric {

  implicit object ByteIsExactNumeric extends ExactNumeric[Byte] {
    val n = ByteIsIntegral
    override def plus(x: Byte, y: Byte): Byte = x.addExact(y)
    override def minus(x: Byte, y: Byte): Byte = x.subtractExact(y)
    override def times(x: Byte, y: Byte): Byte = x.multiplyExact(y)
  }

  implicit object ShortIsExactNumeric extends ExactNumeric[Short] {
    val n = ShortIsIntegral
    override def plus(x: Short, y: Short): Short = x.addExact(y)
    override def minus(x: Short, y: Short): Short = x.subtractExact(y)
    override def times(x: Short, y: Short): Short = x.multiplyExact(y)
  }

  implicit object IntIsExactNumeric extends ExactNumeric[Int] {
    val n = IntIsIntegral
    override def plus(x: Int, y: Int): Int = Math.addExact(x, y)
    override def minus(x: Int, y: Int): Int = Math.subtractExact(x, y)
    override def times(x: Int, y: Int): Int = Math.multiplyExact(x, y)
  }
  
  implicit object LongIsExactNumeric extends ExactNumeric[Long] {
    val n = LongIsIntegral
    override def plus(x: Long, y: Long): Long = Math.addExact(x, y)
    override def minus(x: Long, y: Long): Long = Math.subtractExact(x, y)
    override def times(x: Long, y: Long): Long = Math.multiplyExact(x, y)
  }
}
