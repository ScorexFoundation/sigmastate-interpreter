package sigmastate.serialization.transformers

import sigmastate.Values.Value
import sigmastate.lang.Terms._
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.{OpCodes, ValueSerializer}
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigmastate.utxo.Slice
import sigmastate.{SCollection, SInt, SType}

case class SliceSerializer(cons: (Value[SCollection[SType]], Value[SInt.type], Value[SInt.type]) => Value[SCollection[SType]])
  extends ValueSerializer[Slice[SType]] {

  override val opCode: OpCode = OpCodes.SliceCode

  override def serialize(obj: Slice[SType], w: SigmaByteWriter): Unit =
    w.putValue(obj.input)
      .putValue(obj.from)
      .putValue(obj.until)

  override def parse(r: SigmaByteReader): Value[SCollection[SType]] = {
    val input = r.getValue().asCollection[SType]
    val from = r.getValue().asValue[SInt.type]
    val until = r.getValue().asValue[SInt.type]
    cons(input, from, until)
  }
}
