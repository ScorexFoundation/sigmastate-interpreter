package sigmastate.serialization

import sigmastate.Values._
import sigmastate._
import sigmastate.serialization.OpCodes._
import sigmastate.utils.{ByteReader, ByteWriter}
import sigmastate.utils.Extensions._

object TaggedVariableSerializer extends ValueSerializer[TaggedVariable[_ <: SType]] {

  override val opCode: OpCode = TaggedVariableCode

  override def serializeBody(obj: TaggedVariable[_ <: SType], w: ByteWriter): Unit =
    w.put(obj.varId)
      .putType(obj.tpe)

  override def parseBody(r: ByteReader): TaggedVariable[_ <: SType] = {
    val varId = r.getByte()
    val tpe = r.getType()
    TaggedVariable(varId, tpe)
  }
}
