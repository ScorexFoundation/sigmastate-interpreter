package sigmastate.serialization

import sigmastate._
import sigmastate.Values._
import sigmastate.serialization.ValueSerializer.Position
import OpCodes._

object TaggedVariableSerializer extends ValueSerializer[TaggedVariable[_ <: SType]] {

  override val opCode: OpCode = TaggedVariableCode

  override def parseBody(bytes: Array[Byte], pos: Position) = {
    val varId = bytes(pos)
    val (tpe, consumed) = STypeSerializer.deserialize(bytes, pos + 1)
    val node = TaggedVariable(varId, tpe)
    (node, consumed + 1)
  }

  override def serializeBody(v: TaggedVariable[_ <: SType]) = {
    val tpeBytes = STypeSerializer.serialize(v.tpe)
    Array(v.varId, tpeBytes: _*)
  }
}
