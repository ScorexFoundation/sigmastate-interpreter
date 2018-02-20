package sigmastate.serialization

import sigmastate.{FalseLeaf, SBoolean}
import sigmastate.SType.TypeCode

object FalseLeafSerializer extends SigmaSerializer[FalseLeaf.type] {
  override val opCode = SigmaSerializer.FalseCode

  val typeCode: TypeCode = SBoolean.typeCode

  override def parseBody = {case (bytes, pos) => (FalseLeaf, 0, typeCode)}

  override def serializeBody = {_ => Array[Byte]()}
}
