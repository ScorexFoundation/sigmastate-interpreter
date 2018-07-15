package sigmastate.serialization.transformers

import sigmastate.SType
import sigmastate.lang.DeserializationSigmaBuilder
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.{OpCodes, ValueSerializer}
import sigmastate.utils.{ByteReader, ByteWriter}
import sigmastate.utxo.DeserializeContext
import sigmastate.utils.Extensions._

object DeserializeContextSerializer extends ValueSerializer[DeserializeContext[SType]] {

  override val opCode: OpCode = OpCodes.DeserializeContextCode

  override def serializeBody(obj: DeserializeContext[SType], w: ByteWriter): Unit =
    w.putType(obj.tpe)
      .put(obj.id)

  override def parseBody(r: ByteReader): DeserializeContext[SType] = {
    val tpe = r.getType()
    val id = r.getByte()
    DeserializationSigmaBuilder.mkDeserializeContext(id, tpe)
      .asInstanceOf[DeserializeContext[SType]]
  }
}
