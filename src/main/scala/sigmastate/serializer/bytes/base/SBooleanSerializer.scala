package sigmastate.serializer.bytes.base

import scorex.core.serialization.Serializer
import sigmastate._
import sigmastate.serializer.bytes.BooleanConstantSerializer
import sigmastate.serializer.bytes.BytesDeserializer._

import scala.util.Try

class SBooleanSerializer(implicit booleanConstant: Serializer[BooleanConstant]) extends Serializer[Value[SBoolean.type]] {
  override def toBytes(v: Value[SBoolean.type]): Array[Byte] = v.bytes

  override def parseBytes(bytes: Array[Byte]): Try[Value[SBoolean.type]] = {
    for {
      (opCode, _) <- shortBytes(bytes)
      v <- opCode match {
        case BooleanConstantSerializer.OpCode => booleanConstant.parseBytes(bytes)
      }
    } yield v
  }
}
