package sigmastate.serialization.transformers

import sigmastate.Values.Value
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.Serializer._
import sigmastate.serialization.{OpCodes, Serializer, ValueSerializer}
import sigmastate.lang.Terms._
import sigmastate._

object UpcastSerializer extends ValueSerializer[Upcast[SNumericType, SNumericType]] {
  override val opCode: OpCode = OpCodes.Upcast

  /**
    * Only the inner value is serialized, the upcast itself will be restored in deserialization
    *
    * @param obj Upcast node
    * @return
    */
  override def serializeBody(obj: Upcast[SNumericType, SNumericType]): Array[Byte] = {
    val w = Serializer.startWriter()
        .putValue(obj.input)
    w.toBytes
  }

  /**
    * Upcast is not deserialized and "restored" (via a typer pass)
    *
    * @param bytes
    * @param pos
    * @return value that was "upcasted" before serialization
    */
  override def parseBody(bytes: Array[Byte], pos: Position): (Value[SNumericType], Consumed) = {
    val r = Serializer.startReader(bytes, pos)
    val input = r.getValue().asNumValue
    (input, r.consumed)
  }
}
