package sigma.types

import sigma.util.Extensions._

case class CBoolean(value: scala.Boolean) extends Boolean {
  override def toByte: Byte = CByte(if (value) 1 else 0)
}

case class CByte(value: scala.Byte) extends Byte {
  override def toInt: Int = CInt(value.toInt)

  override def +(y: Byte): Byte = CByte(value.addExact(y.value))
}

case class CInt(value: scala.Int) extends Int {
  override def toByte: Byte = CByte(value.toByteExact)

  override def +(y: Int): Int = CInt(value + y.value)
}

