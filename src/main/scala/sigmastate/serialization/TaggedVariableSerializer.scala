package sigmastate.serialization

import sigmastate.Values._
import sigmastate._
import sigmastate.serialization.OpCodes._
import sigmastate.utils.Extensions._
import sigmastate.utils.{ByteReader, ByteWriter}

case class TaggedVariableSerializer(cons: (Byte, SType) => Value[SType])
  extends ValueSerializer[TaggedVariable[_ <: SType]] {

  override val opCode: OpCode = TaggedVariableCode

  override def serializeBody(obj: TaggedVariable[_ <: SType], w: ByteWriter): Unit =
    w.put(obj.varId)
      .putType(obj.tpe)

  override def parseBody(r: ByteReader): Value[SType] = {
    val varId = r.getByte()
    val tpe = r.getType()
    cons(varId, tpe)
  }
}
