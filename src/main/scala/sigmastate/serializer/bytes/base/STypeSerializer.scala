package sigmastate.serializer.bytes.base

import scorex.core.serialization.Serializer
import sigmastate._

import scala.util.Try
import scala.reflect.runtime.universe._

class STypeSerializer(implicit sIntSerializer: Serializer[Value[SInt.type]],
                                                sBooleanSerializer: Serializer[Value[SBoolean.type]],
                                                sBigIntSerializer: Serializer[Value[SBigInt.type]],
                                                sByteArraySerializer: Serializer[Value[SByteArray.type]],
                                                sPropSerializer: Serializer[Value[SProp.type]],
                                                sAvlTreeSerializer: Serializer[Value[SAvlTree.type]],
                                                sGroupElementSerializer: Serializer[Value[SGroupElement.type]],
                                                sBoxSerializer: Serializer[Value[SBox.type]],
                                                sCollectionSerializer: Serializer[Value[SCollection[SType]]]
                                           ) extends Serializer[Value[SType]] {
  override def toBytes(v: Value[SType]): Array[Byte] = v match {
    case v: Value[SInt.type] => sIntSerializer.toBytes(v)
    case v: Value[SBoolean.type] => sBooleanSerializer.toBytes(v)
    case v: Value[SBigInt.type] => sBigIntSerializer.toBytes(v)
    case v: Value[SByteArray.type] => sByteArraySerializer.toBytes(v)
    case v: Value[SProp.type] => sPropSerializer.toBytes(v)
    case v: Value[SAvlTree.type] => sAvlTreeSerializer.toBytes(v)
    case v: Value[SGroupElement.type] => sGroupElementSerializer.toBytes(v)
    case v: Value[SBox.type] => sBoxSerializer.toBytes(v)
    case v: Value[SCollection[SType]] => sCollectionSerializer.toBytes(v)
  }

  override def parseBytes(bytes: Array[Byte]): Try[Value[SType]] = {
    def aux(tpe: Type): Try[Value[SType]] = {
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
      r.map(_.asInstanceOf[Value[SType]])
    }
    aux(implicitly[WeakTypeTag[SType]].tpe)
  }
}
