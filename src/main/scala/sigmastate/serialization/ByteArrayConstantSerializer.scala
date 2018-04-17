package sigmastate.serialization

import com.google.common.primitives.Ints
import sigmastate.Values.ByteArrayConstant

import scala.collection.mutable.ArrayBuffer

object ByteArrayConstantSerializer extends ValueSerializer[ByteArrayConstant]{
  import ValueSerializer._

  override val opCode: OpCode = ValueSerializer.ByteArrayConstantCode

  val IntSize = Integer.BYTES

  override def parseBody(bytes: Array[Byte], pos: Position) = {

    val sizeBuffer = bytes.slice(pos, pos + IntSize)
    val size: Int = Ints.fromByteArray(sizeBuffer)

    val valueBuffer = bytes.slice(pos + IntSize,  pos + IntSize + size)

    (ByteArrayConstant(valueBuffer), pos + IntSize + size)
  }

  override def serializeBody(obj: ByteArrayConstant): Array[Byte] = {

    val array = obj.value
    val length = array.length

    require(length <= Int.MaxValue, "max array size is Int.MaxValue")

    val lengthBytes = Ints.toByteArray(length)

    val b = ArrayBuffer[Byte]()
    b ++= lengthBytes
    b ++= array

    b.toArray
  }
}