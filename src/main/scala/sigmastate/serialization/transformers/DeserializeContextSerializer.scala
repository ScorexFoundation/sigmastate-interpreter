package sigmastate.serialization.transformers

import sigmastate.SType
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.{OpCodes, ValueSerializer}
import sigmastate.utils.{ByteReader, ByteWriterSigmaValues}
import sigmastate.utxo.DeserializeContext

object DeserializeContextSerializer extends ValueSerializer[DeserializeContext[SType]] {

  override val opCode: OpCode = OpCodes.DeserializeContextCode

  override def serializeBody(obj: DeserializeContext[SType], w: ByteWriterSigmaValues): Unit =
    w.putType(obj.tpe)
      .put(obj.id)

  override def parseBody(r: ByteReader): DeserializeContext[SType] = {
    val tpe = r.getType()
    val id = r.getByte()
    DeserializeContext(id, tpe)
  }
}
