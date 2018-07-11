package sigmastate.serialization

import sigmastate.SType
import sigmastate.Values.Value
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.utils.{ByteReader, ByteWriterSigmaValues}

case class CaseObjectSerialization[V <: Value[SType]](override val opCode: OpCode, obj: V)
  extends ValueSerializer[V] {

  override def serializeBody(obj: V, w: ByteWriterSigmaValues): Unit = ()

  override def parseBody(r: ByteReader): V = obj
}
