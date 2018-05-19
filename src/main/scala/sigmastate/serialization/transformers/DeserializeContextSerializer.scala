package sigmastate.serialization.transformers

import sigmastate.SType
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.Serializer._
import sigmastate.serialization.{OpCodes, ValueSerializer}
import sigmastate.utxo.DeserializeContext

object DeserializeContextSerializer extends ValueSerializer[DeserializeContext[SType]] {

  override val opCode: OpCode = OpCodes.DeserializeContextCode

  override def serializeBody(obj: DeserializeContext[SType]): Array[Byte] = {
    Array(obj.tpe.typeCode, obj.id)
  }

  override def parseBody(bytes: Array[Byte], pos: Position): (DeserializeContext[SType], Consumed) = {
    val tpeByte = bytes(pos)
    val tpe = SType.allPredefTypes.filter(_.typeCode == tpeByte).head
    val id = bytes(pos + 1)
    (DeserializeContext(id)(tpe), 2)
  }

}
