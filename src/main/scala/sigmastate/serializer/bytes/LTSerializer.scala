package sigmastate.serializer.bytes

import com.google.common.primitives.Bytes
import scorex.core.serialization.Serializer
import sigmastate.serializer.bytes.BytesDeserializer._
import sigmastate.serializer.bytes.BytesSerializer._
import sigmastate.{LT, SInt, Value}

import scala.util.Try

object LTSerializer {
  val OpCode: Short = 1
}
class LTSerializer(implicit intSerializer: Serializer[Value[SInt.type]]) extends Serializer[LT] {
  import LTSerializer.OpCode
  override def toBytes(lt: LT): Array[Byte] = {
    Bytes.concat(
      shortBytesEnsureCapacity(OpCode),
      arrayWithKnownSize(lt.left.bytes),
      arrayWithKnownSize(lt.right.bytes)
    )
  }

  override def parseBytes(bytes: Array[Byte]): Try[LT] = {
    for {
      (opCode, bytesAfterOpCode) <- shortBytes(bytes)
      if opCode == OpCode
      (leftBytes, bytesAfterLeft) <- arrayWithoutKnownSize(bytesAfterOpCode)
      left <- intSerializer.parseBytes(leftBytes)
      (rightBytes, bytesAfterRight) <- arrayWithoutKnownSize(bytesAfterLeft)
      right <- intSerializer.parseBytes(rightBytes)
    } yield {
      require(bytesAfterRight.isEmpty)
      LT(left, right)
    }
  }
}
