package sigmastate.serialization

import java.nio.ByteBuffer

import sigmastate._
import sigmastate.utils.Extensions._
import sigmastate.SType.TypeCode
import sigmastate.serialization.Serializer.{Position}
import sigmastate.utils.ByteArrayBuilder

trait ByteBufferSerializer[T] {
  def serialize(tpe: SType, buf: ByteArrayBuilder): Unit
  def deserialize(buf: ByteBuffer): SType
}

object STypeSerializer extends ByteBufferSerializer[SType] {

  override def serialize(tpe: SType, buf: ByteArrayBuilder) = tpe match {
    case p: SPrimType =>
      buf.append(p.typeCode)
    case c: SCollection[a] => c.elemType match {
      case p: SPrimType =>
//        buf.append(SCollection.)

    }
  }

  override def deserialize(buf: ByteBuffer): SType = {
    val c = buf.get().toInt
    val tpe: SType = if (c <= 0)
      sys.error(s"Cannot deserialize type prefix $c. Unexpected buffer $buf with bytes ${buf.getBytes(buf.remaining())}")
    else if (c <= SPrimType.MaxPrimTypeCode)
      c.toByte match {
        case SPrimType(t) => t
        case _ => sys.error(s"Cannot deserialize primitive type with code $c")
      }
    else //if (c <= SCollection.MaxCollectionTypeCode)
      sys.error(s"Invalid type prefix $c")
    tpe
  }
}

///** All primitive types are processed by the same serializer */
//class PrimitiveTypeSerializer(val typeCode: TypeCode) extends STypeSerializer[SType] {
//  override val companion = STypeSerializer
//
//  override def parseBody(bytes: Array[Byte], pos: Position) = typeCode match {
//    case SPrimType(t) => (t, 0)
//    case _ => sys.error(s"Don't know how to parse primitive type descriptor with typeCode = $typeCode")
//  }
//
//  override def serializeBody(tpe: SType) = tpe match {
//    case t if t.isPrimitive => Array[Byte]()
//    case _ => sys.error(s"Don't know how to serialize primitive type descriptor $tpe")
//  }
//}

//object SCollectionSerializer extends STypeSerializer[SCollection[SType]] {
//  override val companion = STypeSerializer
//  val typeCode = SCollection.CollectionTypeCode
//
//  override def parseBody(bytes: Array[Byte], pos: Position) = {
//    val (tElem, len) = STypeSerializer.deserialize(bytes, pos)
//    (SCollection(tElem), len)
//  }
//
//  override def serializeBody(tpe: SCollection[SType]) = {
//    STypeSerializer.serialize(tpe.elemType)
//  }
//}