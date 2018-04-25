package sigmastate.serialization

import sigmastate.serialization.ValueSerializer.Position
import sigmastate.utxo.Height

object HeightSerializer extends ValueSerializer[Height.type ] {
  override val opCode = OpCodes.HeightCode

  override def parseBody(bytes: Array[Byte], pos: Position) = (Height, 0)

  override def serializeBody(obj: Height.type ) = Array[Byte]()
}
