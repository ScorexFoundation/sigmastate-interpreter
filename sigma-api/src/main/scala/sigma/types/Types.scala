package sigma.types

import scalan.{Internal, Nullable}
import special.collection.Coll

@Internal
private[types] trait PrimView[@specialized Val] {
  private[types] def value: Val
}

trait Boolean extends PrimView[scala.Boolean] {
  /** Convert true to 1 and false to 0
    * @since 2.0
    */
  def toByte: Byte

  /** Logical XOR
    * @since 2.0
    */
  def xor(y: Boolean): Boolean
}

trait Byte extends PrimView[scala.Byte] {
//  def toShort: Short
  def toInt: Int
//  def toLong: Long
  def + (y: Byte): Byte
  /** Negation
    * @since 2.0
    */
  def negate : Byte
}

trait Int extends PrimView[scala.Int] {
  def toByte: Byte
  def + (y: Int): Int

  /** Returns a big-endian representation of this Int in a collection of bytes.
    * For example, the Int value {@code 0x12131415} would yield the
    * byte array {@code {0x12, 0x13, 0x14, 0x15}}.
    * @since 2.0
    */
  def toBytes: Coll[Byte]

  /** Returns a big-endian representation of this numeric in a collection of Booleans.
    * Each boolean corresponds to one bit.
    * @since 2.0
    */
  def toBits: Coll[Boolean]

  /** Absolute value of this numeric value.
    * @since 2.0
    */
  def abs: Int

  /** Compares this numeric with that numeric for order. Returns a negative integer, zero, or a positive integer as the
    * `this` is less than, equal to, or greater than `that`.
    */
  def compareTo(that: Int): Int

  /** Negation
    * @since 2.0
    */
  def negate : Int
}


