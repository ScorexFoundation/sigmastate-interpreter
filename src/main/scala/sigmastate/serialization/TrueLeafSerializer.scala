package sigmastate.serialization

import sigmastate.SType.TypeCode
import sigmastate.{SBoolean, TrueLeaf}

object TrueLeafSerializer extends SigmaSerializer[TrueLeaf.type] {
  override val opCode = SigmaSerializer.TrueCode

  override val typeCode: TypeCode = SBoolean.typeCode

  override def parseBody = {case (bytes, pos) => TrueLeaf -> 0}

  override def serializeBody = {cc => ???}
}
