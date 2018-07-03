package sigmastate.serialization.transformers

import sigmastate.SType
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.{OpCodes, ValueSerializer}
import sigmastate.utils.{ByteReader, ByteWriter}
import sigmastate.utxo.DeserializeContext

object DeserializeContextSerializer extends ValueSerializer[DeserializeContext[SType]] {

  override val opCode: OpCode = OpCodes.DeserializeContextCode

  override def parseBody(r: ByteReader): DeserializeContext[SType] = {
    val tpeByte = r.getByte()
    val tpe = SType.allPredefTypes.filter(_.typeCode == tpeByte).head
    val id = r.getByte()
    DeserializeContext(id, tpe)
  }

  override def serializeBody(obj: DeserializeContext[SType], w: ByteWriter): Unit =
    w.put(obj.tpe.typeCode)
      .put(obj.id)
}
