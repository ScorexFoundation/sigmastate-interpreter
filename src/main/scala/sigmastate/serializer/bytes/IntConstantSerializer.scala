package sigmastate.serializer.bytes

import com.google.common.primitives.Bytes
import scorex.core.serialization.Serializer
import sigmastate.IntConstant
import sigmastate.serializer.bytes.BytesDeserializer._
import sigmastate.serializer.bytes.BytesSerializer._

import scala.util.Try

object IntConstantSerializer {
  private val OpCode: Short = 22
}
class IntConstantSerializer extends Serializer[IntConstant] {
  import IntConstantSerializer._

  override def toBytes(c: IntConstant): Array[Byte] = {
    Bytes.concat(
      shortBytesEnsureCapacity(OpCode),
      longBytesEnsureCapacity(c.value)
    )
  }

  override def parseBytes(bytes: Array[Byte]): Try[IntConstant] = {
    for {
      (opCode, bytesAfterOpCode) <- shortBytes(bytes)
      if opCode == OpCode
      (value, bytesAfterConstant) <- longBytes(bytesAfterOpCode)
    } yield {
      IntConstant(value)
    }
  }
}
