package sigmastate.serialization.transformers

import sigmastate.Values.Value
import sigmastate.lang.Terms._
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.{OpCodes, ValueSerializer}
import sigmastate.serialization.ValueSerializer
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigmastate.utxo.Slice
import sigmastate.{SInt, SCollection, SType}

case class SliceSerializer(cons: (Value[SCollection[SType]], Value[SInt.type], Value[SInt.type]) => Value[SCollection[SType]])
  extends ValueSerializer[Slice[SType]] {
  import sigmastate.Operations.SliceInfo._
  override def opDesc = Slice

  override def serialize(obj: Slice[SType], w: SigmaByteWriter): Unit =
    w.putValue(obj.input, thisArg)
      .putValue(obj.from, fromArg)
      .putValue(obj.until, untilArg)

  override def parse(r: SigmaByteReader): Value[SCollection[SType]] = {
    val input = r.getValue().asCollection[SType]
    val from = r.getValue().asValue[SInt.type]
    val until = r.getValue().asValue[SInt.type]
    cons(input, from, until)
  }
}
