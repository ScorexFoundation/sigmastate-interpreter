package sigmastate.serializer.bytes

import com.google.common.primitives.Bytes
import scorex.core.serialization.Serializer
import sigmastate.BooleanConstant
import sigmastate.serializer.bytes.BytesDeserializer._
import sigmastate.serializer.bytes.BytesSerializer.{booleanToByte, shortBytesEnsureCapacity}

import scala.util.Try

object BooleanConstantSerializer {
  val OpCode: Short = 35
}
class BooleanConstantSerializer extends Serializer[BooleanConstant] {
  import BooleanConstantSerializer._
  override def toBytes(c: BooleanConstant): Array[Byte] = {
    Bytes.concat(
      shortBytesEnsureCapacity(OpCode),
      Array(booleanToByte(c.value))
    )
  }

  override def parseBytes(bytes: Array[Byte]): Try[BooleanConstant] = {
    for {
      (opCode, bytesAfterOpCode) <- shortBytes(bytes)
      if opCode == OpCode
      (value, bytesAfterConstant) <- booleanFromByte(bytesAfterOpCode)
    } yield {
      BooleanConstant.fromBoolean(value)
    }
  }
}
