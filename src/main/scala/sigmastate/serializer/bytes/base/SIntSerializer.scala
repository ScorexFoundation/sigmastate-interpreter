package sigmastate.serializer.bytes.base

import scorex.core.serialization.Serializer
import sigmastate._
import sigmastate.serializer.bytes.BytesDeserializer._
import sigmastate.serializer.bytes.{HeightSerializer, IntConstantSerializer}

import scala.util.Try

class SIntSerializer(implicit intConstantSerializer: Serializer[IntConstant],
                heightSerializer: HeightSerializer) extends Serializer[Value[SInt.type]] {

  override def toBytes(v: Value[SInt.type]): Array[Byte] = v.bytes

  override def parseBytes(bytes: Array[Byte]): Try[Value[SInt.type]] = {
    for {
      (opCode, _) <- shortBytes(bytes)
      v <- opCode match {
        case IntConstantSerializer.OpCode => intConstantSerializer.parseBytes(bytes)
        case HeightSerializer.OpCode => heightSerializer.parseBytes(bytes)
      }
    } yield v
  }
}
