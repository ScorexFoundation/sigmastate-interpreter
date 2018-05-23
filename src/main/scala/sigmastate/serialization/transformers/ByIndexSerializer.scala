package sigmastate.serialization.transformers

import sigmastate.Values.Value
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.Serializer.{Position, Consumed}
import sigmastate.serialization.{OpCodes, ValueSerializer}
import sigmastate.utxo.ByIndex
import sigmastate.{SCollection, SInt, SType}

object ByIndexSerializer extends ValueSerializer[ByIndex[SType]] {

  override val opCode: OpCode = OpCodes.ByIndexCode

  override def parseBody(bytes: Array[Byte], pos: Position): (ByIndex[SType], Consumed) = {
    val (input, c1) = ValueSerializer.deserialize(bytes, pos)
    val (index, c2) = ValueSerializer.deserialize(bytes, pos + c1)
    ByIndex(input.asInstanceOf[Value[SCollection[SType]]],
      index.asInstanceOf[Value[SInt.type]]) -> (c1 + c2)
  }

  override def serializeBody(obj: ByIndex[SType]): Array[Byte] = {
    ValueSerializer.serialize(obj.input) ++ ValueSerializer.serialize(obj.index)
  }
}
