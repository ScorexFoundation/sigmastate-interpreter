package sigmastate.serialization.transformers

import sigmastate.Values.Value
import sigmastate.lang.Terms._
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.{OpCodes, ValueSerializer}
import sigma.util.Extensions._
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigmastate.utxo.Append
import sigmastate.{SCollection, SType}

case class AppendSerializer(cons: (Value[SCollection[SType]], Value[SCollection[SType]]) => Value[SCollection[SType]])
  extends ValueSerializer[Append[SType]] {

  override val opCode: OpCode = OpCodes.AppendCode

  override def serializeBody(obj: Append[SType], w: SigmaByteWriter): Unit =
    w.putValue(obj.input)
      .putValue(obj.col2)

  override def parseBody(r: SigmaByteReader): Value[SCollection[SType]] = {
    val input = r.getValue().asCollection[SType]
    val col2 = r.getValue().asCollection[SType]
    cons(input, col2)
  }
}
