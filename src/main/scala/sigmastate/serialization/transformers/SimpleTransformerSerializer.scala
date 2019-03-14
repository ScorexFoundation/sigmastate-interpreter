package sigmastate.serialization.transformers

import sigmastate.SType
import sigmastate.Values.Value
import sigmastate.lang.Terms._
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.ValueSerializer
import sigmastate.utils.{SerializeLog, SigmaByteReader, SigmaByteWriter}
import sigmastate.utxo.Transformer
import sigma.util.Extensions._

case class SimpleTransformerSerializer[I <: SType, O <: SType]
(code: OpCode,
 cons: Value[I] => Value[O]) extends ValueSerializer[Transformer[I, O]] {

  override val opCode: OpCode = code

  override def serializeBody(obj: Transformer[I, O], w: SigmaByteWriter): Unit = {
    SerializeLog.logPrintf(true, true, false, "SimpleTransformer")

    SerializeLog.logPrintf(true, true, false, "input")
    w.putValue(obj.input)
    SerializeLog.logPrintf(false, true, false, "input")

    SerializeLog.logPrintf(false, true, false, "SimpleTransformer")
  }

  override def parseBody(r: SigmaByteReader): Value[O] =
    cons(r.getValue().asValue[I])
}
