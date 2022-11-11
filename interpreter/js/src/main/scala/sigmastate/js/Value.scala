package sigmastate.js

import scorex.util.Extensions.{IntOps, LongOps}
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

@JSExportTopLevel("Value", moduleID = "core")
class Value(val data: Any, val tpe: Type) extends js.Object {

}

@JSExportTopLevel("Values", moduleID = "core")
object Value extends js.Object {
  val MaxLong = js.BigInt("0x7fffffffffffffff")
  val MinLong = -js.BigInt("0x8000000000000000")

  def ofByte(n: Int): Value = {
    new Value(n.toByteExact, Type.Byte)
  }
  def ofShort(n: Int): Value = {
    new Value(n.toShortExact, Type.Short)
  }
  def ofInt(n: Int): Value = {
    new Value(n.toLong.toIntExact, Type.Int)
  }
  def ofLong(n: js.BigInt): Value = {
    if (n < MinLong || n > MaxLong)
      throw new ArithmeticException(s"value $n is out of long range")
    new Value(n, Type.Long)
  }
}
