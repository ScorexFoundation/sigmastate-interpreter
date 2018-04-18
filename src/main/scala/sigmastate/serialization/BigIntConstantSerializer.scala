package sigmastate.serialization

import java.math.BigInteger

import com.google.common.primitives.Ints
import sigmastate.SBigInt
import sigmastate.SType.TypeCode
import sigmastate.Values.BigIntConstant
import sigmastate.serialization.ValueSerializer._

object BigIntConstantSerializer extends ValueSerializer[BigIntConstant] {

  val IntSize: Int = Integer.BYTES

  override val opCode: OpCode = ValueSerializer.BigIntConstantCode

  val typeCode: TypeCode = SBigInt.typeCode

  override def parseBody(bytes: Array[Byte], pos: Position): (BigIntConstant, Position) = {

    val sizeBuffer = bytes.slice(pos, pos + IntSize)
    val size: Int = Ints.fromByteArray(sizeBuffer)

    val valueBuffer = bytes.slice(pos + IntSize,  pos + IntSize + size)
    BigIntConstant(new BigInteger(valueBuffer)) -> (IntSize + size)
  }

  override def serializeBody(obj: BigIntConstant): Array[Byte] = {

    val data: Array[Byte] = obj.value.toByteArray
    val length = data.length

    require(length <= Int.MaxValue, "max collection size is Int.MaxValue")

    Ints.toByteArray(length) ++ data
  }
}
