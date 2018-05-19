package sigmastate.serialization.transformers

import sigmastate.Values.Value
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.Serializer._
import sigmastate.serialization.{OpCodes, ValueSerializer}
import sigmastate.utxo.Where
import sigmastate.{SBoolean, SCollection, SType}

object WhereSerializer extends ValueSerializer[Where[SType]] {

  override val opCode: OpCode = OpCodes.WhereCode

  override def serializeBody(obj: Where[SType]): Array[Byte] = {
    obj.id +: (ValueSerializer.serialize(obj.input) ++ ValueSerializer.serialize(obj.condition))
  }

  override def parseBody(bytes: Array[Byte], pos: Position): (Where[SType], Consumed) = {
    val id = bytes(pos)
    val (input, consumed) = ValueSerializer.deserialize(bytes, pos + 1)
    val (condition, consumed2) = ValueSerializer.deserialize(bytes, pos + consumed + 1)
    (Where(input.asInstanceOf[Value[SCollection[SType]]], id, condition.asInstanceOf[Value[SBoolean.type]]),
      consumed + consumed2)
  }

}
