package sigmastate.serialization

import sigmastate.SType
import sigmastate.Values.Value
import sigmastate.serialization.OpCodes.OpCode

import sigmastate.serialization.Serializer.{Position, Consumed}

case class CaseObjectSerialization[V <: Value[SType]](override val opCode: OpCode, obj: V)
  extends ValueSerializer[V] {

  override def parseBody(bytes: Array[Byte], pos: Position): (V, Consumed) = (obj, 0)

  override def serializeBody(obj: V): Array[Byte] = Array.emptyByteArray
}
