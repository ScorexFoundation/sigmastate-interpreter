package sigma.data

import debox.cfor
import sigma.Evaluation.stypeToRType
import sigma.{BigInt, Coll, Colls}
import sigma.data.ExactIntegral._
import sigma.data.RType.SomeType

import scala.collection.mutable

/** Numeric operations with overflow checks.
  * Raise exception when overflow is detected.
  * Each instance of this typeclass overrides three methods `plus`, `minus`, `times`.
  * All other methods are implemented by delegating to the corresponding Numeric instance from
  * standard Scala library.
  * This trait is used in core IR to avoid implicitly using standard scala implementations
  */
trait ExactNumeric[T] {
  protected val n: Numeric[T]

  /** Addition operation `x + y`. */
  def plus(x: T, y: T): T

  /** Subtraction operation `x - y`. */
  def minus(x: T, y: T): T

  /** Multiplication operation `x * y`. */
  def times(x: T, y: T): T

  /** Returns negative value `-x`. */
  def negate(x: T): T = n.negate(x)

  /** Returns a value of type T, which corresponds to the given integer value `x`. */
  def fromInt(x: Int): T = n.fromInt(x)

  def toInt(x: T): Int = n.toInt(x)
  def toLong(x: T): Long = n.toLong(x)

  /** Returns a big-endian representation of this value in a collection of bytes.
    * For example, the `Int` value `0x12131415` would yield the
    * collection of bytes [0x12, 0x13, 0x14, 0x15]
    */
  def toBigEndianBytes(x: T): Coll[Byte]

  def toBits(x: T): Coll[Boolean] = {

    def isBitSet(byte: Byte)(bit: Int): Boolean = ((byte >> bit) & 1) == 1

    def byte2Bools(b: Byte): Array[Boolean] =  (0 to 7).toArray.reverse.map(isBitSet(b))

    val bytes = toBigEndianBytes(x)
    val l = bytes.length
    val res = new Array[Boolean](l * 8)
    var offset = 0
    cfor(0)(_ < l, _ + 1) { i =>
      val b = bytes(i)
      val bits = byte2Bools(b)
      Array.copy(bits, 0, res, offset, 8)
      offset += 8
    }
    Colls.fromArray(res)
  }

  def bitwiseInverse(x: T): T

  def bitwiseOr(x: T, y: T): T

  def bitwiseAnd(x: T, y: T): T

  def bitwiseXor(x: T, y: T): T

  def shiftLeft(x: T, bits: Int): T

  def shiftRight(x: T, bits: Int): T

  /** A value of type T which corresponds to integer 0. */
  lazy val zero: T = fromInt(0)

  /** A value of type T which corresponds to integer 1. */
  lazy val one: T = fromInt(1)
}

object ExactNumeric {
  /** ExactNumeric instances are the same as ExactIntegral.
    * This values are implicitly used wherever ExactNumeric is needed (see usages).
    */
  implicit def IntIsExactNumeric: ExactNumeric[Int] = IntIsExactIntegral
  implicit def LongIsExactNumeric: ExactNumeric[Long] = LongIsExactIntegral
}
