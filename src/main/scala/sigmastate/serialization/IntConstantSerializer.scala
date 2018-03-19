package sigmastate.serialization

import com.google.common.primitives.Longs
import sigmastate.Values._
import sigmastate.{SInt}
import sigmastate.SType.TypeCode

object IntConstantSerializer extends ValueSerializer[IntConstant] {
  import ValueSerializer._
  override val opCode = ValueSerializer.IntConstantCode

  val typeCode: TypeCode = SInt.typeCode

  override def parseBody(bytes: Array[Byte], pos: Position) = {
    (IntConstant(Longs.fromByteArray(bytes.slice(pos, pos + 8))), 8)
  }

  override def serializeBody(c: IntConstant) = Longs.toByteArray(c.value)
}
