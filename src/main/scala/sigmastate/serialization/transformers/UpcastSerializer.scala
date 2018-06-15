package sigmastate.serialization.transformers

import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.Serializer._
import sigmastate.serialization.{ValueSerializer, OpCodes, Serializer}
import sigmastate.lang.Terms._
import sigmastate._

object UpcastSerializer extends ValueSerializer[Upcast[SNumericType, SNumericType]] {
  override val opCode: OpCode = OpCodes.Upcast

  override def serializeBody(obj: Upcast[SNumericType, SNumericType]): Array[Byte] = {
    val w = Serializer.startWriter()
        .putValue(obj.input)
        .putType(obj.tpe)
    w.toBytes
  }

  override def parseBody(bytes: Array[Byte], pos: Position): (Upcast[SNumericType, SNumericType], Consumed) = {
    val r = Serializer.startReader(bytes, pos)
    val input = r.getValue().asNumValue
    val tpe = r.getType().asNumType
    (Upcast(input, tpe), r.consumed)
  }
}
