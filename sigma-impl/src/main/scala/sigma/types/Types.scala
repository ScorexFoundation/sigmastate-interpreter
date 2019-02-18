package sigma.types

import com.google.common.primitives.Ints
import scorex.util.Extensions._
import special.collection.{Coll, Builder}

case class CBoolean(value: scala.Boolean) extends Boolean {
  override def toByte: Byte = CByte(if (value) 1 else 0)
}

case class CByte(value: scala.Byte) extends Byte {
  override def toInt: Int = CInt(value.toInt)

  override def +(y: Byte): Byte = CByte(value.addExact(y.value))
}

case class CInt(value: scala.Int) extends Int {
  override def toByte: Byte = CByte(value.toByteExact)
  override def +(that: Int): Int = CInt(value + that.value)

  /** Returns a big-endian representation of this Int in a collection of bytes.
    * For example, the Int value {@code 0x12131415} would yield the
    * byte array {@code {0x12, 0x13, 0x14, 0x15}}.
    * @since 2.0
    */
  def toBytes: Coll[Byte] = ??? //Builder.DefaultCollBuilder.viewColl(Ints.toByteArray(value))

  /** Returns a big-endian representation of this numeric in a collection of Booleans.
    * Each boolean corresponds to one bit.
    * @since 2.0
    */
  def toBits: Coll[Boolean] = ???

  /** Absolute value of this numeric value.
    * @since 2.0
    */
  def abs: Int = CInt(if (value < 0) -value else value)

  /** Compares this numeric with that numeric for order. Returns a negative integer, zero, or a positive integer as the
    * `this` is less than, equal to, or greater than `that`.
    */
  override def compareTo(that: Int): Int = CInt(if (value < that.value) -1 else if (value == that.value) 0 else 1)
}

