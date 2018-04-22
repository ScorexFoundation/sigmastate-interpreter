package sigmastate.serialization
    
import com.google.common.primitives.Shorts
import sigmastate.SByteArray
import sigmastate.SType.TypeCode
import sigmastate.Values.ByteArrayConstant
import sigmastate.serialization.OpCodes._
import sigmastate.serialization.ValueSerializer.{Consumed, Position}

object ByteArrayConstantSerializer extends ValueSerializer[ByteArrayConstant] {

  override val opCode: OpCode = ByteArrayConstantCode
  val typeCode: TypeCode = SByteArray.typeCode

  override def parseBody(bytes: Array[Byte], pos: Position): (ByteArrayConstant, Consumed) = {
    val length = Shorts.fromByteArray(bytes.slice(pos, pos + 2))
    val consumed = 2 + length
    val array = bytes.slice(pos + 2 , pos + consumed)
    (ByteArrayConstant(array), consumed)
  }

  override def serializeBody(arr: ByteArrayConstant): Array[Byte] = {
    val lengthBytes = Shorts.toByteArray(arr.value.length.toShort)
    lengthBytes ++ arr.value
  }
}
