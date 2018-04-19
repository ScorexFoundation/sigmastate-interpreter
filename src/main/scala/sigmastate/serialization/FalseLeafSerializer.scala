package sigmastate.serialization

import sigmastate.Values._
import sigmastate.{SBoolean}
import sigmastate.SType.TypeCode
import sigmastate.serialization.ValueSerializer.Position
import sigmastate.serialization.OpCodes._

object FalseLeafSerializer extends ValueSerializer[FalseLeaf.type] {
  override val opCode = FalseCode

  val typeCode: TypeCode = SBoolean.typeCode

  override def parseBody(bytes: Array[Byte], pos: Position) = (FalseLeaf, 0)

  override def serializeBody(obj: FalseLeaf.type) = Array[Byte]()
}
