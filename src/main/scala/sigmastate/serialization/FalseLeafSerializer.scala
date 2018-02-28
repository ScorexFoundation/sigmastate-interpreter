package sigmastate.serialization

import sigmastate.{FalseLeaf, SBoolean}
import sigmastate.SType.TypeCode
import sigmastate.serialization.ValueSerializer.Position

object FalseLeafSerializer extends ValueSerializer[FalseLeaf.type] {
  override val opCode = ValueSerializer.FalseCode

  val typeCode: TypeCode = SBoolean.typeCode

  override def parseBody(bytes: Array[Byte], pos: Position) = (FalseLeaf, 0)

  override def serializeBody(obj: FalseLeaf.type) = Array[Byte]()
}
