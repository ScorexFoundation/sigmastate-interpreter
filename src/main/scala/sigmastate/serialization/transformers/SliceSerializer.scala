package sigmastate.serialization.transformers

import sigmastate.Values.Value
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.Serializer._
import sigmastate.serialization.{OpCodes, ValueSerializer}
import sigmastate.utxo.Slice
import sigmastate.{SCollection, SInt, SType}

object SliceSerializer extends ValueSerializer[Slice[SType]] {

  override val opCode: OpCode = OpCodes.SliceCode

  override def serializeBody(obj: Slice[SType]): Array[Byte] = {
    ValueSerializer.serialize(obj.input) ++ ValueSerializer.serialize(obj.from) ++ ValueSerializer.serialize(obj.until)
  }

  override def parseBody(bytes: Array[Byte], pos: Position): (Slice[SType], Consumed) = {
    val (input, consumed) = ValueSerializer.deserialize(bytes, pos)
    val (from, consumed2) = ValueSerializer.deserialize(bytes, pos + consumed)
    val (until, consumed3) = ValueSerializer.deserialize(bytes, pos + consumed + consumed2)
    (Slice(input.asInstanceOf[Value[SCollection[SType]]], from.asInstanceOf[Value[SInt.type]],
      until.asInstanceOf[Value[SInt.type]]), consumed + consumed2 + consumed3)
  }
}
