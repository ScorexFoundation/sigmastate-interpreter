package sigmastate.serialization

import sigmastate._
import sigmastate.utils.{ByteWriter, ByteReader}

trait ByteBufferSerializer[T] {
  def serialize(tpe: T, buf: ByteWriter): Unit
  def deserialize(buf: ByteReader): T
}

object TypeSerializer extends ByteBufferSerializer[SType] {

  override def serialize(tpe: SType, w: ByteWriter) = tpe match {
    case p: SPrimType =>
      w.put(p.typeCode)
    case c: SCollection[a] => c.elemType match {
      case p: SPrimType =>
        val code = (SCollection.CollectionTypeCode + p.typeCode).toByte
        w.put(code)
    }
  }

  val primIdToType = Array[SType](NoType, SByte, SBoolean, SInt, SBigInt, SGroupElement, SAvlTree, SBox)

  def getPrimType(code: Int): SType =
    if (code <= 0 || code >= primIdToType.length)
      sys.error(s"Cannot deserialize primitive type with code $code")
    else
      primIdToType(code)

  override def deserialize(r: ByteReader): SType = {
    val c = r.get().toInt
    if (c <= 0)
      sys.error(s"Cannot deserialize type prefix $c. Unexpected buffer $r with bytes ${r.getBytes(r.remaining)}")
    val tpe: SType = if (c < STuple.TupleTypeCode) {
      val constrId = c / SPrimType.PrimRange
      val primId   = c % SPrimType.PrimRange
      constrId match {
        case 0 => // primitive
          getPrimType(c)
        case 1 => // Array[_]
          val tElem = if (primId == 0) {
            deserialize(r)
          } else {
            getPrimType(primId)
          }
          SCollection(tElem)
      }
    }
    else {//if (c == STuple.TupleTypeCode) {
      NoType
    }
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