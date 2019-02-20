package sigmastate.serialization.transformers

import sigmastate.SType
import sigmastate.Values.Value
import sigmastate.lang.Terms._
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.ValueSerializer
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigmastate.utxo.Transformer
import scorex.util.Extensions._

case class SimpleTransformerSerializer[I <: SType, O <: SType]
(code: OpCode,
 cons: Value[I] => Value[O]) extends ValueSerializer[Transformer[I, O]] {

  override val opCode: OpCode = code

  override def serialize(obj: Transformer[I, O], w: SigmaByteWriter): Unit =
    w.putValue(obj.input)

  override def parse(r: SigmaByteReader): Value[O] =
    cons(r.getValue().asValue[I])
}
