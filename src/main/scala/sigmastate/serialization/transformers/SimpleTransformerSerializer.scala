package sigmastate.serialization.transformers

import sigmastate.SType
import sigmastate.Values.Value
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.ValueSerializer
import sigmastate.utxo.Transformer
import sigmastate.serialization.Serializer.{Position, Consumed}

case class SimpleTransformerSerializer[I <: SType, O <: SType]
(code: OpCode,
 cons: Value[I] => Transformer[I, O]) extends ValueSerializer[Transformer[I, O]] {

  override val opCode: OpCode = code

  override def parseBody(bytes: Array[OpCode], pos: Position): (Transformer[I, O], Consumed) = {
    val (input, consumed) = ValueSerializer.deserialize(bytes, pos)
    cons(input.asInstanceOf[Value[I]]) -> consumed
  }

  override def serializeBody(obj: Transformer[I, O]): Array[OpCode] = {
    ValueSerializer.serialize(obj.input)
  }
}
