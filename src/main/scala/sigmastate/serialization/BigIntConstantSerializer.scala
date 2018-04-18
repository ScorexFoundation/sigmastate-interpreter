package sigmastate.serialization

import sigmastate.SBigInt
import sigmastate.SType.TypeCode
import sigmastate.Values.BigIntConstant
import sigmastate.serialization.ValueSerializer.OpCode
import sigmastate.serialization.ValueSerializer.Position

object BigIntConstantSerializer extends ValueSerializer[BigIntConstant] {
  override val opCode: OpCode = ValueSerializer.BigIntConstantCode
  val typeCode: TypeCode = SBigInt.typeCode

  override def parseBody(bytes: Array[Byte], pos: Position) = {
    val length = bytes(pos)
    val bIntArray = bytes.slice(pos + 1, pos + 1 + length)
    val bInt = BigInt.apply(bIntArray)
    BigIntConstant(bInt.bigInteger) -> (length + 1)
  }

  override def serializeBody(obj: BigIntConstant): Array[Byte] = {
    val data = obj.value.toByteArray
    data.length.toByte +: data
  }
}
