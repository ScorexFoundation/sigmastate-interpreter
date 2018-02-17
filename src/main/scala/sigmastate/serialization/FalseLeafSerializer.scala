package sigmastate.serialization

import sigmastate.{FalseLeaf, SBoolean}
import sigmastate.SType.TypeCode

object FalseLeafSerializer extends SigmaSerializer[FalseLeaf.type] {
  override val opCode = SigmaSerializer.FalseCode

  override val typeCode: TypeCode = SBoolean.typeCode

  override def parseBody = {case (bytes, pos) => FalseLeaf -> 0}

  override def serializeBody = {cc => ???}
}
