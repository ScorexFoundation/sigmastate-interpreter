package sigmastate.serialization

import com.google.common.primitives.Longs
import sigmastate.IntConstant

object IntConstantSerializer extends SigmaSerializer[IntConstant] {
  override val opCode = SigmaSerializer.IntConstantCode

  override def parseBody = {
    case (bytes, pos) =>
      IntConstant(Longs.fromByteArray(bytes.slice(pos, pos + 8))) -> 8
  }

  override def serializeBody = (c => Longs.toByteArray(c.value))
}
