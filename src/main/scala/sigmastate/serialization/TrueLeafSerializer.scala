package sigmastate.serialization

import sigmastate.TrueLeaf

object TrueLeafSerializer extends SigmaSerializer[TrueLeaf.type] {
  override val opCode = SigmaSerializer.TrueCode

  override def parseBody = {case (bytes, pos) => TrueLeaf -> 0}

  override def serializeBody = {cc => ???}
}
