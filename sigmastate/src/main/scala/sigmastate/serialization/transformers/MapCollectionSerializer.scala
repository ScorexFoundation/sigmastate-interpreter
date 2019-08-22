package sigmastate.serialization.transformers

import sigmastate.Values.Value
import sigmastate.lang.Terms._
import sigmastate.serialization.ValueSerializer
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigmastate.utxo.MapCollection
import sigmastate.{SCollection, SType, SFunc}

case class MapCollectionSerializer(cons: (Value[SCollection[SType]], Value[SFunc]) => Value[SType])
  extends ValueSerializer[MapCollection[SType, SType]] {
  import sigmastate.Operations.MapCollectionInfo._
  override def opDesc = MapCollection

  override def serialize(obj: MapCollection[SType, SType], w: SigmaByteWriter): Unit =
    w.putValue(obj.input, thisArg)
      .putValue(obj.mapper, fArg)

  override def parse(r: SigmaByteReader): Value[SType] = {
    val input = r.getValue().asValue[SCollection[SType]]
    val mapper = r.getValue().asFunc
    cons(input, mapper)
  }

}
