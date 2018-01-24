package sigmastate.serializer.bytes

import scorex.core.serialization.Serializer
import sigmastate._

import scala.reflect.runtime.universe._
import scala.util

package object base {
  implicit lazy val sIntSerializer: Serializer[Value[SInt.type]] = new SIntSerializer
  implicit lazy val sBooleanSerializer: Serializer[Value[SBoolean.type]] = new SBooleanSerializer
  implicit lazy val sBigIntSerializer: Serializer[Value[SBigInt.type]] = new Serializer[Value[SBigInt.type]] {override def toBytes(obj: Value[SBigInt.type]): Array[Byte] = ???

    override def parseBytes(bytes: Array[Byte]): util.Try[Value[SBigInt.type]] = ???
  }
  implicit lazy val sByteArraySerializer: Serializer[Value[SByteArray.type]] = new Serializer[Value[SByteArray.type]] {override def toBytes(obj: Value[SByteArray.type]): Array[Byte] = ???

    override def parseBytes(bytes: Array[Byte]): util.Try[Value[SByteArray.type]] = ???
  }
  implicit lazy val sPropSerializer: Serializer[Value[SProp.type]] = new Serializer[Value[SProp.type]] {override def toBytes(obj: Value[SProp.type]): Array[Byte] = ???

    override def parseBytes(bytes: Array[Byte]): util.Try[Value[SProp.type]] = ???
  }
  implicit lazy val sAvlTreeSerializer: Serializer[Value[SAvlTree.type]] = new Serializer[Value[SAvlTree.type]] {override def toBytes(obj: Value[SAvlTree.type]): Array[Byte] = ???

    override def parseBytes(bytes: Array[Byte]): util.Try[Value[SAvlTree.type]] = ???
  }
  implicit lazy val sGroupElementSerializer: Serializer[Value[SGroupElement.type]] = new Serializer[Value[SGroupElement.type]] {override def toBytes(obj: Value[SGroupElement.type]): Array[Byte] = ???

    override def parseBytes(bytes: Array[Byte]): util.Try[Value[SGroupElement.type]] = ???
  }
  implicit lazy val sBoxSerializer: Serializer[Value[SBox.type]] = new Serializer[Value[SBox.type]] {override def toBytes(obj: Value[SBox.type]): Array[Byte] = ???

    override def parseBytes(bytes: Array[Byte]): util.Try[Value[SBox.type]] = ???
  }
  implicit def sTypeSerializer[T <: SType : TypeTag]: Serializer[Value[T]] =
    new STypeSerializer[T]

  implicit val sCollectionSerializer: SCollectionSerializer[SInt.type] = new SCollectionSerializer[SInt.type]
//  implicit val sCollectionSerializer = new SCollectionSerializer[SInt.type]
//  implicit def sCollectionSerializer[T <: SType: TypeTag](implicit d: Serializer[Value[T]]): Serializer[Value[SCollection[T]]] = new SCollectionSerializer[T]
}
