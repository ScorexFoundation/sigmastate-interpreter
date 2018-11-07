package sigmastate.serialization

import sigmastate.SType
import sigmastate.Values.Value
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}

case class CaseObjectSerialization[V <: Value[SType]](override val opCode: OpCode, obj: V)
  extends ValueSerializer[V] {

  override def serializeBody(obj: V, w: SigmaByteWriter): Unit = ()

  override def parseBody(r: SigmaByteReader): V = obj
}
