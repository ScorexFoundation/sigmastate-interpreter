package sigmastate.serialization.transformers

import com.google.common.primitives.Ints
import sigmastate.Values.Value
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.ValueSerializer._
import sigmastate.serialization.{OpCodes, ValueSerializer}
import sigmastate.utxo.ByIndex
import sigmastate.{SCollection, SType}

object ByIndexSerializer extends ValueSerializer[ByIndex[SType]] {

  override val opCode: OpCode = OpCodes.ByIndexCode
  val intLength = Ints.BYTES

  override def parseBody(bytes: Array[Byte], pos: Position): (ByIndex[SType], Consumed) = {
    val (input, consumed) = ValueSerializer.deserialize(bytes, pos)
    val index = Ints.fromByteArray(bytes.slice(pos + consumed, pos + consumed + intLength))
    ByIndex(input.asInstanceOf[Value[SCollection[SType]]], index) -> (consumed + intLength)
  }

  override def serializeBody(obj: ByIndex[SType]): Array[Byte] = {
    ValueSerializer.serialize(obj.input) ++ Ints.toByteArray(obj.index)
  }
}
