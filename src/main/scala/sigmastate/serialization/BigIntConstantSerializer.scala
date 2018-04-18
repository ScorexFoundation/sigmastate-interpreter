package sigmastate.serialization

import java.math.BigInteger

import com.google.common.primitives.{Ints, Shorts}
import sigmastate.SBigInt
import sigmastate.SType.TypeCode
import sigmastate.Values.BigIntConstant
import sigmastate.serialization.ValueSerializer._

object BigIntConstantSerializer extends ValueSerializer[BigIntConstant] {

  val lengthSize: Int = 2

  override val opCode: OpCode = ValueSerializer.BigIntConstantCode

  val typeCode: TypeCode = SBigInt.typeCode

  override def parseBody(bytes: Array[Byte], pos: Position): (BigIntConstant, Position) = {

    val sizeBuffer = bytes.slice(pos, pos + lengthSize)
    val size: Short = Shorts.fromByteArray(sizeBuffer)

    val valueBuffer = bytes.slice(pos + lengthSize,  pos + lengthSize + size)
    BigIntConstant(new BigInteger(valueBuffer)) -> (lengthSize + size)
  }

  override def serializeBody(obj: BigIntConstant): Array[Byte] = {

    val data: Array[Byte] = obj.value.toByteArray
    val length = data.length

    require(length <= Short.MaxValue, "max collection size is Short.MaxValue")

    Shorts.toByteArray(length.toShort) ++ data
  }
}
