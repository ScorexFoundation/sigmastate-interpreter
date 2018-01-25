package sigmastate.serializer.bytes.base

import scorex.core.serialization.Serializer
import sigmastate._
import sigmastate.serializer.bytes.BytesDeserializer._
import sigmastate.serializer.bytes.ConcreteCollectionSerializer

import scala.util.Try

class SCollectionSerializer(implicit concreteCollectionConstant: ConcreteCollectionSerializer) extends Serializer[Value[SCollection[SType]]] {
  override def toBytes(obj: Value[SCollection[SType]]): Array[Byte] = {
    obj match {
      case c@ConcreteCollection(_) => concreteCollectionConstant.toBytes(c)
    }
  }

  override def parseBytes(bytes: Array[Byte]): Try[Value[SCollection[SType]]] = {
    for {
      (opCode, _) <- shortBytes(bytes)
      v <- opCode match {
        case ConcreteCollectionSerializer.OpCode =>
          for {
            parsed <- concreteCollectionConstant.parseBytes(bytes)
          } yield parsed
      }
    } yield v
  }
}
