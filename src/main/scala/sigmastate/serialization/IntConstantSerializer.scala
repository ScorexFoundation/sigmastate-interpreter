package sigmastate.serialization

import com.google.common.primitives.Longs
import sigmastate.{IntConstant, SInt}
import sigmastate.SType.TypeCode

object IntConstantSerializer extends SigmaSerializer[IntConstant] {
  override val opCode = SigmaSerializer.IntConstantCode

  val typeCode: TypeCode = SInt.typeCode

  override def parseBody = {
    case (bytes, pos) =>
      (IntConstant(Longs.fromByteArray(bytes.slice(pos, pos + 8))), 8, typeCode)
  }

  override def serializeBody = (c => Longs.toByteArray(c.value))
}
