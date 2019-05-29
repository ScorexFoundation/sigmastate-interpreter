package sigmastate.serialization

import sigmastate.Values._
import sigmastate._
import sigmastate.serialization.OpCodes._
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}

case class TaggedVariableSerializer(cons: (Byte, SType) => Value[SType])
  extends ValueSerializer[TaggedVariable[_ <: SType]] {
  override def opDesc = TaggedVariable
  override def opCode: OpCode = TaggedVariableCode

  override def serialize(obj: TaggedVariable[_ <: SType], w: SigmaByteWriter): Unit =
    w.put(obj.varId)
      .putType(obj.tpe)

  override def parse(r: SigmaByteReader): Value[SType] = {
    val varId = r.getByte()
    val tpe = r.getType()
    cons(varId, tpe)
  }
}
