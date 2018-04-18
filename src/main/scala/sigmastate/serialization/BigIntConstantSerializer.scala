package sigmastate.serialization

import java.math.BigInteger

import com.google.common.primitives.Ints
import sigmastate.SBigInt
import sigmastate.SType.TypeCode
import sigmastate.Values.BigIntConstant
import sigmastate.serialization.ValueSerializer._

import scala.collection.mutable.ArrayBuffer

object BigIntConstantSerializer extends ValueSerializer[BigIntConstant] {

  val IntSize = Integer.BYTES

  override val opCode: OpCode = ValueSerializer.BigIntConstantCode

  val typeCode: TypeCode = SBigInt.typeCode

  override def parseBody(bytes: Array[Byte], pos: Position): (BigIntConstant, Position) = {

    val sizeBuffer = bytes.slice(pos, pos + IntSize)
    val size: Int = Ints.fromByteArray(sizeBuffer)

    val valueBuffer = bytes.slice(pos + IntSize,  pos + IntSize + size)
    (BigIntConstant(new BigInteger(valueBuffer)), IntSize + size)
  }

  override def serializeBody(c: BigIntConstant): Array[OpCode] = {

    val array = c.value.toByteArray
    val length = array.size

    require(length <= Int.MaxValue, "max collection size is Int.MaxValue")
    val lengthBytes = Ints.toByteArray(length)

    val b = ArrayBuffer[Byte]()
    b ++= lengthBytes
    b ++= array

    b.toArray
  }
}
