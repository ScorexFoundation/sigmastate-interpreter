package sigmastate.serialization

import sigmastate._
import sigmastate.utils.Extensions._
import sigmastate.Values._
import sigmastate.serialization.Serializer.Position
import OpCodes._
import sigmastate.utils.ByteArrayBuilder

object TaggedVariableSerializer extends ValueSerializer[TaggedVariable[_ <: SType]] {

  override val opCode: OpCode = TaggedVariableCode

  override def parseBody(bytes: Array[Byte], pos: Position) = {
    val r = Serializer.startReader(bytes, pos)
    val varId = r.get()
    val tpe = r.getType()
    val node = TaggedVariable(varId, tpe)
    (node, r.consumed)
  }

  override def serializeBody(v: TaggedVariable[_ <: SType]) = {
    val w = Serializer.startWriter()
        .put(v.varId)
        .putType(v.tpe)
    w.toBytes
  }
}
