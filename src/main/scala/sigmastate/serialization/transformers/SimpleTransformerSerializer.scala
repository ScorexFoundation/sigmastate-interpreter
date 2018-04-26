package sigmastate.serialization.transformers

import sigmastate.SType
import sigmastate.Values.Value
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.ValueSerializer
import sigmastate.utxo.Transformer
import sigmastate.serialization.ValueSerializer._

case class SimpleTransformerSerializer[I <: SType, O <: SType, R <: Transformer[I, O]]
  (code: OpCode,
   cons: Value[I] => R) extends ValueSerializer[R] {

  override val opCode: OpCode = code

  override def parseBody(bytes: Array[OpCode], pos: Position): (R, Consumed) = {
    val (input, consumed) = ValueSerializer.deserialize(bytes, pos)
    cons(input.asInstanceOf[Value[I]]) -> consumed
  }

  override def serializeBody(obj: R): Array[OpCode] = {
    ValueSerializer.serialize(obj.input)
  }
}
