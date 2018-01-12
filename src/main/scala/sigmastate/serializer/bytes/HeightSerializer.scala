package sigmastate.serializer.bytes

import scorex.core.serialization.Serializer
import sigmastate.{Height, IntConstant}
import sigmastate.serializer.bytes.BytesDeserializer._
import sigmastate.serializer.bytes.BytesSerializer._

import scala.util.Try

object HeightSerializer {
  val OpCode: Short = 46
}
class HeightSerializer extends Serializer[Height.type] {
  import HeightSerializer._

  override def toBytes(obj: Height.type): Array[Byte] = shortBytesEnsureCapacity(OpCode)

  override def parseBytes(bytes: Array[Byte]): Try[Height.type] = {
    for {
      (opCode, bytesAfterOpCode) <- shortBytes(bytes)
      if opCode == OpCode
    } yield Height
  }
}
