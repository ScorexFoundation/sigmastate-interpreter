package sigmastate.serialization.transformers

import sigmastate.Values.Value
import sigmastate.lang.Terms._
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.{OpCodes, ValueSerializer}
import scorex.util.Extensions._
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigmastate.utxo.Filter
import sigmastate.{SBoolean, SCollection, SType}

case class FilterSerializer(cons: (Value[SCollection[SType]], Byte, Value[SBoolean.type]) => Value[SCollection[SType]]) extends ValueSerializer[Filter[SType]] {

  override val opCode: OpCode = OpCodes.FilterCode

  override def serialize(obj: Filter[SType], w: SigmaByteWriter): Unit =
    w.put(obj.id)
    .putValue(obj.input)
    .putValue(obj.condition)

  override def parse(r: SigmaByteReader): Value[SCollection[SType]] = {
    val id = r.getByte()
    val input = r.getValue().asCollection[SType]
    val condition = r.getValue().asValue[SBoolean.type]
    cons(input, id, condition)
  }
}
