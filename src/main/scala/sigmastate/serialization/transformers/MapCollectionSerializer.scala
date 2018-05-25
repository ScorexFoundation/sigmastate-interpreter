package sigmastate.serialization.transformers

import sigmastate.Values.Value
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.Serializer.{Position, Consumed}
import sigmastate.serialization.{ValueSerializer, OpCodes, Serializer}
import sigmastate.utils.ByteArrayBuilder
import sigmastate.utxo.MapCollection
import sigmastate.{SCollection, SType, Values}
import sigmastate.utils.Extensions._

object MapCollectionSerializer extends ValueSerializer[MapCollection[SType, SType]] {

  override val opCode: OpCode = OpCodes.MapCollectionCode

  override def parseBody(bytes: Array[Byte], pos: Position): (Values.Value[SType], Consumed) = {
    val r = Serializer.startReader(bytes, pos)
    val input = r.getValue.asInstanceOf[Value[SCollection[SType]]]
    val idByte = r.get()
    val mapper = r.getValue
    MapCollection(input, idByte, mapper) -> r.consumed
  }

  override def serializeBody(obj: MapCollection[SType, SType]): Array[Byte] = {
    val w = Serializer.startWriter()
        .putValue(obj.input)
        .put(obj.id)
        .putValue(obj.mapper)
    w.toBytes
  }
}
