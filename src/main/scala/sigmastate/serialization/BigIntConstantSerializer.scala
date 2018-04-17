package sigmastate.serialization

import java.math.BigInteger

import com.google.common.primitives.Ints
import sigmastate.SBigInt
import sigmastate.SType.TypeCode
import sigmastate.Values.BigIntConstant
import sigmastate.serialization.ValueSerializer._

import scala.collection.mutable.ArrayBuffer

object BigIntConstantSerializer extends ValueSerializer[BigIntConstant] {

  val IntSize = 4

  override val opCode: OpCode = ValueSerializer.BigIntConstantCode

  val typeCode: TypeCode = SBigInt.typeCode

  override def parseBody(bytes: Array[Byte], pos: Position): (BigIntConstant, Position) = {

    val sizeBuffer = bytes.slice(pos, pos + IntSize)
    val size: Int = Ints.fromByteArray(sizeBuffer)

    val valueBuffer = bytes.slice(pos + IntSize,  pos + IntSize + size)
    (BigIntConstant(new BigInteger(valueBuffer)), pos + IntSize + size)
  }

  override def serializeBody(c: BigIntConstant): Array[OpCode] = {

    val array = c.value.toByteArray
    val size = array.size
    require(size <= Int.MaxValue, "max collection size is Int.MaxValue")
    val sizeBytes = Ints.toByteArray(array.size)

    val b = ArrayBuffer[Byte]()
    b ++= sizeBytes
    b ++= array

    b.toArray
  }
}
