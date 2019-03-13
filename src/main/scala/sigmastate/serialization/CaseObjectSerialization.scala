package sigmastate.serialization

import sigmastate.SType
import sigmastate.Values.Value
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.utils.{SerializeLog, SigmaByteReader, SigmaByteWriter}

case class CaseObjectSerialization[V <: Value[SType]](override val opCode: OpCode, obj: V)
  extends ValueSerializer[V] {

  override def serializeBody(obj: V, w: SigmaByteWriter): Unit = {
    SerializeLog.logPrintf(true, true, false,"CaseObject")
    SerializeLog.logPrintf(false, true, false,"CaseObject")
  }

  override def parseBody(r: SigmaByteReader): V = obj
}
