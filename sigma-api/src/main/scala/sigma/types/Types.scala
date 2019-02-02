package sigma.types

import scalan.{Internal, Nullable}

@Internal
private[types] trait PrimValue[@specialized Val] {
  private[types] def value: Val
}

trait Boolean extends PrimValue[scala.Boolean] {
  /** Convert true to 1 and false to 0
    * @since 2.0
    */
  def toByte: Byte
}

trait Byte extends PrimValue[scala.Byte] {
//  def toShort: Short
  def toInt: Int
//  def toLong: Long
  def + (y: Byte): Byte
}

trait Int extends PrimValue[scala.Int] {
  def toByte: Byte
  def + (y: Int): Int
}


