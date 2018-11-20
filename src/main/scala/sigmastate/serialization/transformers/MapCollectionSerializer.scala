package sigmastate.serialization.transformers

import sigmastate.Values.Value
import sigmastate.lang.Terms._
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.{OpCodes, ValueSerializer}
import scorex.util.Extensions._
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigmastate.utxo.MapCollection
import sigmastate.{SCollection, SType}

case class MapCollectionSerializer(cons: (Value[SCollection[SType]], Byte, Value[SType]) => Value[SType])
  extends ValueSerializer[MapCollection[SType, SType]] {

  override val opCode: OpCode = OpCodes.MapCollectionCode

  override def serialize(obj: MapCollection[SType, SType], w: SigmaByteWriter): Unit =
    w.putValue(obj.input)
      .put(obj.id)
      .putValue(obj.mapper)

  override def parse(r: SigmaByteReader): Value[SType] = {
    val input = r.getValue().asValue[SCollection[SType]]
    val idByte = r.getByte()
    val mapper = r.getValue()
    cons(input, idByte, mapper)
  }

}
