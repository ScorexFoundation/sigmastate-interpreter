package sigmastate.serialization

import sigmastate.SType.TypeCode
import sigmastate._
import sigmastate.serialization.STypeSerializer.Position

import scala.util.Try

trait STypeSerializer[T <: SType] extends SigmaSerializer[SType, T] {

  import STypeSerializer._

  val typeCode: TypeCode

  override def toBytes(obj: T): Array[Byte] = serialize(obj)

  override def parseBytes(bytes: Array[Byte]): Try[T] = Try {
    deserialize(bytes, 0).asInstanceOf[T]
  }
}

object STypeSerializer extends SigmaSerializerCompanion[SType] {
  override type Tag = SType.TypeCode
  val table: Map[Tag, SigmaSerializer[SType, _]] = {
    val primSers = SType.allPredefTypes.map(t => (t.typeCode, new PrimitiveTypeSerializer(t.typeCode))).toMap
    primSers + (SCollection.TypeCode -> SCollectionSerializer)
  }

  override def deserialize(bytes: Array[TypeCode], pos: Position): (SType, Consumed) = {
    val c = bytes(pos)
    val handler = table(c)
    val (tpe, consumed) = handler.parseBody(bytes, pos + 1)
    (tpe, consumed + 1)
  }

  override def serialize(tpe: SType) = {
    val tc = tpe.typeCode
    val ser = table(tc).asInstanceOf[SigmaSerializer[SType, SType]]
    tc +: ser.serializeBody(tpe)
  }
}

/** All primitive types are processed by the same serializer */
class PrimitiveTypeSerializer(val typeCode: TypeCode) extends STypeSerializer[SType] {
  override val companion = STypeSerializer

  override def parseBody(bytes: Array[Byte], pos: Position) = typeCode match {
    case SPrimType(t) => (t, 0)
    case _ => sys.error(s"Don't know how to parse primitive type descriptor with typeCode = $typeCode")
  }

  override def serializeBody(tpe: SType) = tpe match {
    case t if t.isPrimitive => Array[Byte]()
    case _ => sys.error(s"Don't know how to serialize primitive type descriptor $tpe")
  }
}

object SCollectionSerializer extends STypeSerializer[SCollection[SType]] {
  override val companion = STypeSerializer
  val typeCode = SCollection.TypeCode

  override def parseBody(bytes: Array[Byte], pos: Position) = {
    val (tElem, len) = STypeSerializer.deserialize(bytes, pos)
    (SCollection(tElem), len)
  }

  override def serializeBody(tpe: SCollection[SType]) = {
    STypeSerializer.serialize(tpe.elemType)
  }
}