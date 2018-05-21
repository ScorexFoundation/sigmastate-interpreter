package sigmastate.serialization

import com.google.common.primitives.Longs
import sigmastate.{SByte}
import sigmastate.SType.TypeCode
import sigmastate.Values._
import sigmastate.serialization.OpCodes._
import sigmastate.serialization.Serializer.Position

object ByteConstantSerializer extends ValueSerializer[ByteConstant] {
  override val opCode = ByteConstantCode

  val typeCode: TypeCode = SByte.typeCode

  override def parseBody(bytes: Array[Byte], pos: Position) = {
    (ByteConstant(bytes(pos)), 1)
  }

  override def serializeBody(c: ByteConstant) = Array[Byte](c.value)
}

