package sigma.types

import scalan.Internal

@Internal
trait SType[@specialized Val] {
  private[types] def value: Val
}

trait Boolean extends SType[scala.Boolean] {
  def toByte: Byte
}

trait Byte extends SType[scala.Byte] {
  def toInt: Int
  def + (y: Byte): Byte
}

trait Int extends SType[scala.Int] {
  def toByte: Byte
  def + (y: Int): Int
}


