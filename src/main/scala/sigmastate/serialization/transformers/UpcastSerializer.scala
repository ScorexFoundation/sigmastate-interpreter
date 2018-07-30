package sigmastate.serialization.transformers

import sigmastate.Values.Value
import sigmastate._
import sigmastate.lang.Terms._
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.{OpCodes, ValueSerializer}
import sigmastate.utils.{ByteReader, ByteWriter}
import sigmastate.utils.Extensions._

case class UpcastSerializer(cons: (Value[SNumericType], SNumericType) => Value[SNumericType])
  extends ValueSerializer[Upcast[SNumericType, SNumericType]] {
  override val opCode: OpCode = OpCodes.Upcast

  override def serializeBody(obj: Upcast[SNumericType, SNumericType], w: ByteWriter): Unit =
    w.putValue(obj.input)
      .putType(obj.tpe)

  override def parseBody(r: ByteReader): Value[SNumericType] = {
    val input = r.getValue().asNumValue
    val tpe = r.getType().asNumType
    cons(input, tpe)
  }
}
