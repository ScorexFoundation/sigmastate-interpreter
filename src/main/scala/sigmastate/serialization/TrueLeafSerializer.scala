package sigmastate.serialization

import sigmastate.SType.TypeCode
import sigmastate.Values._
import sigmastate.serialization.ValueSerializer.Position
import sigmastate.{SBoolean}

object TrueLeafSerializer extends ValueSerializer[TrueLeaf.type] {
  override val opCode = ValueSerializer.TrueCode

  val typeCode: TypeCode = SBoolean.typeCode

  override def parseBody(bytes: Array[Byte], pos: Position) = {(TrueLeaf, 0)}

  override def serializeBody(obj: TrueLeaf.type) = {Array[Byte]()}
}
