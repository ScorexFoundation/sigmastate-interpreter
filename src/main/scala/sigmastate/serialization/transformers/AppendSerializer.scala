package sigmastate.serialization.transformers

import sigmastate.Values.Value
import sigmastate.lang.Terms._
import sigmastate.serialization.ValueSerializer
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigmastate.utxo.Append
import sigmastate.{SCollection, SType}

case class AppendSerializer(cons: (Value[SCollection[SType]], Value[SCollection[SType]]) => Value[SCollection[SType]])
  extends ValueSerializer[Append[SType]] {
  override def opDesc = Append

  override def serialize(obj: Append[SType], w: SigmaByteWriter): Unit =
    w.putValue(obj.input, "input")
      .putValue(obj.col2, "other")

  override def parse(r: SigmaByteReader): Value[SCollection[SType]] = {
    val input = r.getValue().asCollection[SType]
    val col2 = r.getValue().asCollection[SType]
    cons(input, col2)
  }
}
