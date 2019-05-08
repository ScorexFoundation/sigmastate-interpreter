package sigmastate.serialization.transformers

import sigmastate.Values.Value
import sigmastate.lang.Terms._
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.{OpCodes, ValueSerializer}
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigmastate.utxo.Filter
import sigmastate.{SCollection, SFunc, SType}

case class FilterSerializer(cons: (Value[SCollection[SType]], Value[SFunc]) => Value[SCollection[SType]]) extends ValueSerializer[Filter[SType]] {

  override val opCode: OpCode = OpCodes.FilterCode

  override def serialize(obj: Filter[SType], w: SigmaByteWriter): Unit =
    w.putValue(obj.input)
    .putValue(obj.condition)

  override def parse(r: SigmaByteReader): Value[SCollection[SType]] = {
    val input = r.getValue().asCollection[SType]
    val condition = r.getValue().asFunc
    cons(input, condition)
  }
}
