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
trait ExactIntegral[T] extends Integral[T] {
  val n: Integral[T]
  override def quot(x: T, y: T): T = n.quot(x, y)
  override def rem(x: T, y: T): T = n.rem(x, y)
  override def negate(x: T): T = n.negate(x)
  override def fromInt(x: Int): T = n.fromInt(x)
  override def toInt(x: T): Int = n.toInt(x)
  override def toLong(x: T): Long = n.toLong(x)
  override def toFloat(x: T): Float = n.toFloat(x)
  override def toDouble(x: T): Double = n.toDouble(x)
  override def compare(x: T, y: T): Int = n.compare(x, y)
}

/** ExactNumeric instances for all types. */
object ExactIntegral {

  implicit object ByteIsExactIntegral extends ExactIntegral[Byte] {
    val n = ByteIsIntegral
    override def plus(x: Byte, y: Byte): Byte = x.addExact(y)
    override def minus(x: Byte, y: Byte): Byte = x.subtractExact(y)
    override def times(x: Byte, y: Byte): Byte = x.multiplyExact(y)
  }

  implicit object ShortIsExactIntegral extends ExactIntegral[Short] {
    val n = ShortIsIntegral
    override def plus(x: Short, y: Short): Short = x.addExact(y)
    override def minus(x: Short, y: Short): Short = x.subtractExact(y)
    override def times(x: Short, y: Short): Short = x.multiplyExact(y)
  }

  implicit object IntIsExactIntegral extends ExactIntegral[Int] {
    val n = IntIsIntegral
    override def plus(x: Int, y: Int): Int = Math.addExact(x, y)
    override def minus(x: Int, y: Int): Int = Math.subtractExact(x, y)
    override def times(x: Int, y: Int): Int = Math.multiplyExact(x, y)
  }

  implicit object LongIsExactIntegral extends ExactIntegral[Long] {
    val n = LongIsIntegral
    override def plus(x: Long, y: Long): Long = Math.addExact(x, y)
    override def minus(x: Long, y: Long): Long = Math.subtractExact(x, y)
    override def times(x: Long, y: Long): Long = Math.multiplyExact(x, y)
  }
}
