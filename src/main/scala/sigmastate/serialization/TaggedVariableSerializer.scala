package sigmastate.serialization

import sigmastate.Values._
import sigmastate._
import sigmastate.serialization.OpCodes._
import sigmastate.utils.{ByteReaderSigmaValues, ByteWriterSigmaValues}

object TaggedVariableSerializer extends ValueSerializer[TaggedVariable[_ <: SType]] {

  override val opCode: OpCode = TaggedVariableCode

  override def serializeBody(obj: TaggedVariable[_ <: SType], w: ByteWriterSigmaValues): Unit =
    w.put(obj.varId)
      .putType(obj.tpe)

  override def parseBody(r: ByteReaderSigmaValues): TaggedVariable[_ <: SType] = {
    val varId = r.getByte()
    val tpe = r.getType()
    TaggedVariable(varId, tpe)
  }
}
