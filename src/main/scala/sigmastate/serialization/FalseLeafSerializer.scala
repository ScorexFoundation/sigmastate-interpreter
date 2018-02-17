package sigmastate.serialization

import sigmastate.FalseLeaf

object FalseLeafSerializer extends SigmaSerializer[FalseLeaf.type] {
  override val opCode = SigmaSerializer.FalseCode

  override def parseBody = {case (bytes, pos) => FalseLeaf -> 0}

  override def serializeBody = {cc => ???}
}
