package sigmastate.serialization.transformers

import sigmastate.Values.Value
import sigmastate.lang.Terms._
import sigmastate.serialization.ValueSerializer
import ValueSerializer._
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigmastate.utxo.ByIndex
import sigmastate.{SInt, SCollection, SType}

case class ByIndexSerializer(cons: (Value[SCollection[SType]], Value[SInt.type], Option[Value[SType]]) => Value[SType])
  extends ValueSerializer[ByIndex[SType]] {
  override def opDesc = ByIndex

  override def serialize(obj: ByIndex[SType], w: SigmaByteWriter): Unit = {
    w.putValue(obj.input, "input")
        .putValue(obj.index, "index")
    opt(w, "default", obj.default)(_.putValue(_, "value"))
  }

  override def parse(r: SigmaByteReader): Value[SType] = {
    val input = r.getValue().asCollection[SType]
    val index = r.getValue().upcastTo(SInt)
    val default = r.getOption(r.getValue())
    cons(input, index, default)
  }

}
