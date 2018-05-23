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
    val buf = Serializer.start(bytes, pos)
    val input = buf.getValue.asInstanceOf[Value[SCollection[SType]]]
    val idByte = buf.get()
    val mapper = buf.getValue
    MapCollection(input, idByte, mapper) -> (buf.position() - pos)
  }

  override def serializeBody(obj: MapCollection[SType, SType]): Array[Byte] = {
    val b = new ByteArrayBuilder()
        .appendValue(obj.input)
        .append(obj.id)
        .appendValue(obj.mapper)
    b.toBytes
  }
}
