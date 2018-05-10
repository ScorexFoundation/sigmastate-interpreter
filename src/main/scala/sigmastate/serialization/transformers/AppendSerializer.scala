package sigmastate.serialization.transformers

import sigmastate.Values.Value
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.ValueSerializer._
import sigmastate.serialization.{OpCodes, ValueSerializer}
import sigmastate.utxo.Append
import sigmastate.{SCollection, SType}

object AppendSerializer extends ValueSerializer[Append[SType]] {

  override val opCode: OpCode = OpCodes.AppendCode

  override def serializeBody(obj: Append[SType]): Array[Byte] = {
    ValueSerializer.serialize(obj.input) ++ ValueSerializer.serialize(obj.col2)
  }

  override def parseBody(bytes: Array[Byte], pos: Position): (Append[SType], Consumed) = {
    val (input, consumed) = ValueSerializer.deserialize(bytes, pos)
    val (col2, consumed2) = ValueSerializer.deserialize(bytes, pos + consumed)
    (Append(input.asInstanceOf[Value[SCollection[SType]]], col2.asInstanceOf[Value[SCollection[SType]]]), consumed + consumed2)
  }

}
