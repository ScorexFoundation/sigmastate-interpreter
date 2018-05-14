package sigmastate.serialization

import java.math.BigInteger

import com.google.common.primitives.{Ints, Shorts}
import sigmastate.SBigInt
import sigmastate.SType.TypeCode
import sigmastate.Values.BigIntConstant
import sigmastate.serialization.OpCodes._
import sigmastate.serialization.ValueSerializer._

object BigIntConstantSerializer extends ValueSerializer[BigIntConstant] {

  val LengthSize: Int = 2

  override val opCode: OpCode = BigIntConstantCode

  val typeCode: TypeCode = SBigInt.typeCode

  override def parseBody(bytes: Array[Byte], pos: Position): (BigIntConstant, Position) = {

    val sizeBuffer = bytes.slice(pos, pos + LengthSize)
    val size: Short = Shorts.fromByteArray(sizeBuffer)

    val valueBuffer = bytes.slice(pos + LengthSize, pos + LengthSize + size)
    BigIntConstant(new BigInteger(valueBuffer)) -> (LengthSize + size)
  }

  override def serializeBody(obj: BigIntConstant): Array[Byte] = {

    val data: Array[Byte] = obj.value.toByteArray
    val length = data.length

    require(length <= Short.MaxValue, "max collection size is Short.MaxValue")

    Shorts.toByteArray(length.toShort) ++ data
  }
}
