package sigmastate.serialization

import sigmastate._
import sigmastate.utils.{ByteWriter, ByteReader}

/** Serialization of types according to specification in TypeSerialization.md. */
object TypeSerializer extends ByteBufferSerializer[SType] {

  val primIdToType = Array[SType](NoType, SByte, SBoolean, SInt, SBigInt, SGroupElement, SAvlTree, SBox)

  def getPrimType(code: Int): SType =
    if (code <= 0 || code >= primIdToType.length)
      sys.error(s"Cannot deserialize primitive type with code $code")
    else
      primIdToType(code)

  override def serialize(tpe: SType, w: ByteWriter) = tpe match {
    case p: SPrimType =>
      w.put(p.typeCode)
    case c: SCollection[a] => c.elemType match {
      case p: SPrimType =>
        val code = (SCollection.CollectionTypeCode + p.typeCode).toByte
        w.put(code)
      case cn: SCollection[a] => cn.elemType match {
        case p: SPrimType =>
          val code = (SCollection.NestedCollectionTypeCode + p.typeCode).toByte
          w.put(code)
        case t =>
          w.put(SCollection.CollectionTypeCode)
          serialize(cn, w)
      }
      case t =>
        w.put(SCollection.CollectionTypeCode)
        serialize(t, w)
    }
    case o: SOption[a] => o.elemType match {
      case p: SPrimType =>
        val code = (SOption.OptionTypeCode + p.typeCode).toByte
        w.put(code)
      case c: SCollection[a] => c.elemType match {
        case p: SPrimType =>
          val code = (SOption.OptionCollectionTypeCode + p.typeCode).toByte
          w.put(code)
        case t =>
          w.put(SOption.OptionTypeCode)
          serialize(c, w)
      }
      case t =>
        w.put(SOption.OptionTypeCode)
        serialize(t, w)
    }
    case tup @ STuple(Seq(t1, t2)) => (t1, t2) match {
      case (p: SPrimType, _) =>
        if (p == t2) {
          val code = (STuple.PairSymmetricTypeCode + p.typeCode).toByte
          w.put(code)
        } else {
          val code = (STuple.Pair1TypeCode + p.typeCode).toByte
          w.put(code)
          serialize(t2, w)
        }
      case (_, p: SPrimType) =>
        val code = (STuple.Pair2TypeCode + p.typeCode).toByte
        w.put(code)
        serialize(t1, w)
      case _ =>
        serializeTuple(tup, w)
    }
    case tup: STuple =>
      serializeTuple(tup, w)
  }

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
          val tElem = getArgType(r, primId)
          SCollection(tElem)
        case 2 => // Array[Array[_]]
          val tElem = getArgType(r, primId)
          SCollection(SCollection(tElem))
        case 3 => // Option[_]
          val tElem = getArgType(r, primId)
          SOption(tElem)
        case 4 => // Option[Collection[_]]
          val tElem = getArgType(r, primId)
          SOption(SCollection(tElem))
        case 5 => // (_, t2)
          val t1 = getArgType(r, primId)
          val t2 = deserialize(r)
          STuple(t1, t2)
        case 6 => // (t1, _)
          val t2 = getArgType(r, primId)
          val t1 = deserialize(r)
          STuple(t1, t2)
        case 7 => // (_, _)
          val t = getArgType(r, primId)
          STuple(t, t)
      }
    }
    else if (c == STuple.TupleTypeCode) {
      val len = r.get()
      val items = (0 until len).map(_ => deserialize(r))
      STuple(items)
    } else {
      sys.error(s"Cannot deserialize type starting from code $c")
    }
    tpe
  }

  private def getArgType(r: ByteReader, primId: Int) =
    if (primId == 0)
      deserialize(r)
    else
      getPrimType(primId)

  private def serializeTuple(t: STuple, w: ByteWriter) = {
    assert(t.items.length <= 255)
    w.put(STuple.TupleTypeCode)
    w.put(t.items.length.toByte)
    for (i <- t.items)
      serialize(i, w)
  }
}

