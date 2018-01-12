package sigmastate.serializer.bytes

import scorex.core.serialization.Serializer
import sigmastate.serializer.bytes.BytesDeserializer._
import sigmastate._

import scala.util.Try

class
SIntSerializer(implicit intConstantSerializer: Serializer[IntConstant],
                heightSerializer: HeightSerializer) extends Serializer[Value[SInt.type]] {
  override def toBytes(v: Value[SInt.type]): Array[Byte] = v match {
    case v: IntConstant => intConstantSerializer.toBytes(v)
    case v: Height.type => heightSerializer.toBytes(v)
  }

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
