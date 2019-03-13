package sigmastate.serialization

import sigmastate.Values._
import sigmastate._
import sigmastate.serialization.OpCodes._
import sigmastate.utils.{SerializeLog, SigmaByteReader, SigmaByteWriter}

case class TaggedVariableSerializer(cons: (Byte, SType) => Value[SType])
  extends ValueSerializer[TaggedVariable[_ <: SType]] {

  override val opCode: OpCode = TaggedVariableCode

  override def serializeBody(obj: TaggedVariable[_ <: SType], w: SigmaByteWriter): Unit = {
    SerializeLog.logPrintf(true, true, false, "TaggedVariable")

    SerializeLog.logPrintf(true, true, false, "VarId")
    w.put(obj.varId)
    SerializeLog.logPrintf(false, true, false, "VarId")

    SerializeLog.logPrintf(true, true, false, "Type")
    w.putType(obj.tpe)
    SerializeLog.logPrintf(false, true, false, "Type")

    SerializeLog.logPrintf(false, true, false, "TaggedVariable")
  }

  override def parseBody(r: SigmaByteReader): Value[SType] = {
    val varId = r.getByte()
    val tpe = r.getType()
    cons(varId, tpe)
  }
}
