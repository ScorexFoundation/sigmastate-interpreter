package sigmastate.serializer.bytes.base

import scorex.core.serialization.Serializer
import sigmastate._
import sigmastate.serializer.bytes.BytesDeserializer._
import sigmastate.serializer.bytes.ConcreteCollectionSerializer

import scala.util.Try

class SCollectionSerializer[T <: SType](implicit concreteCollectionConstant: ConcreteCollectionSerializer[T]) extends Serializer[Value[SCollection[T]]] {
  override def toBytes(obj: Value[SCollection[T]]): Array[Byte] = {
    obj match {
      case c@ConcreteCollection(_) => concreteCollectionConstant.toBytes(c)
    }
  }

  override def parseBytes(bytes: Array[Byte]): Try[Value[SCollection[T]]] = {
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
