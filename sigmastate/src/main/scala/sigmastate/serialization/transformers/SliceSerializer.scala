package sigmastate.serialization.transformers

import sigmastate.Values.{Value, SValue}
import sigmastate.lang.Terms._
import sigmastate.serialization.ValueSerializer
import sigmastate.utils.SigmaByteWriter.DataInfo
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigmastate.utxo.Slice
import sigmastate.{SInt, SCollection, SType}

case class SliceSerializer(cons: (Value[SCollection[SType]], Value[SInt.type], Value[SInt.type]) => Value[SCollection[SType]])
  extends ValueSerializer[Slice[SType]] {
  import sigmastate.Operations.SliceInfo._
  override def opDesc = Slice
  val thisInfo: DataInfo[SValue] = thisArg
  val fromInfo: DataInfo[SValue] = fromArg
  val untilInfo: DataInfo[SValue] = untilArg

  override def serialize(obj: Slice[SType], w: SigmaByteWriter): Unit =
    w.putValue(obj.input, thisInfo)
      .putValue(obj.from, fromInfo)
      .putValue(obj.until, untilInfo)

  override def parse(r: SigmaByteReader): Value[SCollection[SType]] = {
    val input = r.getValue().asCollection[SType]
    val from = r.getValue().asValue[SInt.type]
    val until = r.getValue().asValue[SInt.type]
    cons(input, from, until)
  }
}
