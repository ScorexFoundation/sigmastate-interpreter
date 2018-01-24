package sigmastate.serializer.bytes.base

import scorex.core.serialization.Serializer
import sigmastate._

import scala.util.Try
import scala.reflect.runtime.universe._

class STypeSerializer[T <: SType : TypeTag](implicit sIntSerializer: Serializer[Value[SInt.type]],
                                                sBooleanSerializer: Serializer[Value[SBoolean.type]],
                                                sBigIntSerializer: Serializer[Value[SBigInt.type]],
                                                sByteArraySerializer: Serializer[Value[SByteArray.type]],
                                                sPropSerializer: Serializer[Value[SProp.type]],
                                                sAvlTreeSerializer: Serializer[Value[SAvlTree.type]],
                                                sGroupElementSerializer: Serializer[Value[SGroupElement.type]],
                                                sBoxSerializer: Serializer[Value[SBox.type]],
                                                sCollectionSerializer: Serializer[Value[SCollection[T]]]
                                           ) extends Serializer[Value[T]] {
  override def toBytes(obj: Value[T]): Array[Byte] = obj.bytes

  override def parseBytes(bytes: Array[Byte]): Try[Value[T]] = {
    def aux(tpe: Type): Try[Value[T]] = {
      val r = tpe match {
        case TypeRef(_, col, tps) if col == typeOf[SCollection[_]].typeSymbol => sCollectionSerializer.parseBytes(bytes)
        case t if t == weakTypeOf[SInt.type].resultType => sIntSerializer.parseBytes(bytes)
        case t if t == weakTypeOf[SBigInt.type].resultType => sBigIntSerializer.parseBytes(bytes)
        case t if t == weakTypeOf[SBoolean.type].resultType => sBooleanSerializer.parseBytes(bytes)
        case t if t == weakTypeOf[SByteArray.type].resultType => sByteArraySerializer.parseBytes(bytes)
        case t if t == weakTypeOf[SProp.type].resultType => sPropSerializer.parseBytes(bytes)
        case t if t == weakTypeOf[SAvlTree.type].resultType => sAvlTreeSerializer.parseBytes(bytes)
        case t if t == weakTypeOf[SGroupElement.type].resultType => sGroupElementSerializer.parseBytes(bytes)
        case t if t == weakTypeOf[SBox.type].resultType => sBoxSerializer.parseBytes(bytes)
        case t =>
          ???
      }
      r.map(_.asInstanceOf[Value[T]])
    }
    aux(implicitly[WeakTypeTag[T]].tpe)
  }
}
