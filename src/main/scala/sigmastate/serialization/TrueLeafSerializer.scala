package sigmastate.serialization

import sigmastate.SType.TypeCode
import sigmastate.{SBoolean, TrueLeaf}

object TrueLeafSerializer extends SigmaSerializer[TrueLeaf.type] {
  override val opCode = SigmaSerializer.TrueCode

  val typeCode: TypeCode = SBoolean.typeCode

  override def parseBody = {case (bytes, pos) => (TrueLeaf, 0, typeCode)}

  override def serializeBody = {_ => Array[Byte]()}
}
