package sigmastate.serialization.transformers

import sigmastate.Values.Value
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.Serializer.{Position, Consumed}
import sigmastate.serialization.{OpCodes, ValueSerializer}
import sigmastate.utxo.MapCollection
import sigmastate.{SCollection, SType, Values}

object MapCollectionSerializer extends ValueSerializer[MapCollection[SType, SType]] {

  override val opCode: OpCode = OpCodes.MapCollectionCode

  override def parseBody(bytes: Array[Byte], pos: Position): (Values.Value[SType], Consumed) = {
    val (input, consumed) = ValueSerializer.deserialize(bytes, pos)
    val inputAsCollection = input.asInstanceOf[Value[SCollection[SType]]]
    val idByte = bytes(pos + consumed)
    val (mapper, mapperConsumed) = ValueSerializer.deserialize(bytes, pos + consumed + 1)
    val tOVByteCode = bytes(pos + consumed + 1 + mapperConsumed)
    val tOV = SType.allPredefTypes.filter(_.typeCode == tOVByteCode).head
    MapCollection(inputAsCollection, idByte, mapper) -> (consumed + 1 + mapperConsumed + 1)
  }

  override def serializeBody(obj: MapCollection[SType, SType]): Array[Byte] = {
    val tOV = obj.tOV
    val inputBytes = ValueSerializer.serialize(obj.input)
    val idByte = obj.id
    val mapperBytes = ValueSerializer.serialize(obj.mapper)
    inputBytes ++ Array(idByte) ++ mapperBytes ++ Array(obj.tOV.typeCode)
  }
}
