package sigmastate.serialization.transformers

import sigmastate.Values.{Value, SValue}
import sigmastate.lang.Terms._
import sigmastate.serialization.ValueSerializer
import ValueSerializer._
import sigmastate.Operations.ByIndexInfo._
import sigmastate.utils.SigmaByteWriter.DataInfo
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigmastate.utxo.ByIndex
import sigmastate.{SInt, SCollection, SType}

case class ByIndexSerializer(cons: (Value[SCollection[SType]], Value[SInt.type], Option[Value[SType]]) => Value[SType])
  extends ValueSerializer[ByIndex[SType]] {
  override def opDesc = ByIndex
  val inputInfo: DataInfo[SValue] = thisArg
  val indexInfo: DataInfo[SValue] = indexArg
  val defaultInfo: DataInfo[SValue] = defaultArg

  override def serialize(obj: ByIndex[SType], w: SigmaByteWriter): Unit = {
    w.putValue(obj.input, inputInfo)
        .putValue(obj.index, indexInfo)
    opt(w, "default", obj.default)(_.putValue(_, defaultInfo))
  }

  override def parse(r: SigmaByteReader): Value[SType] = {
    val input = r.getValue().asCollection[SType]
    val index = r.getValue().upcastTo(SInt)
    val default = r.getOption(r.getValue())
    cons(input, index, default)
  }

}
