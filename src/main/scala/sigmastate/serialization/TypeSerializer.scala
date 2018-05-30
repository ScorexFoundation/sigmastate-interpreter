package sigmastate.serialization

import sigmastate._
import sigmastate.utils.{ByteWriter, ByteReader}

/** Serialization of types according to specification in TypeSerialization.md. */
object TypeSerializer extends ByteBufferSerializer[SType] {

  /** The list of embeddable types, i.e. types that can be combined with type constructor for optimized encoding.
    * For each embeddable type `T`, and type constructor `C`, the type `C[T]` can be represented by single byte. */
  val embeddableIdToType = Array[SType](null, SBoolean, SByte, SShort, SInt, SLong, SBigInt, SGroupElement)

  def getEmbeddableType(code: Int): SType =
    if (code <= 0 || code >= embeddableIdToType.length)
      sys.error(s"Cannot deserialize primitive type with code $code")
    else
      embeddableIdToType(code)

  def isEmbeddable(t: SType): Boolean = {
    for (emb <- embeddableIdToType) {
      if (t == emb) return true
    }
    return false
  }

  override def serialize(tpe: SType, w: ByteWriter) = tpe match {
    case p if isEmbeddable(p) =>
      w.put(p.typeCode)
    case SAny => w.put(SAny.typeCode)
    case SUnit => w.put(SUnit.typeCode)
    case SBox => w.put(SBox.typeCode)
    case SAvlTree => w.put(SAvlTree.typeCode)
    case c: SCollection[a] => c.elemType match {
      case p if isEmbeddable(p) =>
        val code = (SCollection.CollectionTypeCode + p.typeCode).toByte
        w.put(code)
      case cn: SCollection[a] => cn.elemType match {
        case p if isEmbeddable(p) =>
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
      case p if isEmbeddable(p) =>
        val code = (SOption.OptionTypeCode + p.typeCode).toByte
        w.put(code)
      case c: SCollection[a] => c.elemType match {
        case p if isEmbeddable(p) =>
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
      case (p, _) if isEmbeddable(p) =>
        if (p == t2) {
          val code = (STuple.PairSymmetricTypeCode + p.typeCode).toByte
          w.put(code)
        } else {
          val code = (STuple.Pair1TypeCode + p.typeCode).toByte
          w.put(code)
          serialize(t2, w)
        }
      case (_, p) if isEmbeddable(p) =>
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
    val c = r.getUByte()
    if (c <= 0)
      sys.error(s"Cannot deserialize type prefix $c. Unexpected buffer $r with bytes ${r.getBytes(r.remaining)}")
    val tpe: SType = if (c < STuple.TupleTypeCode) {
      val constrId = c / SPrimType.PrimRange
      val primId   = c % SPrimType.PrimRange
      constrId match {
        case 0 => // primitive
          getEmbeddableType(c)
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
    else {
      c match {
        case STuple.TupleTypeCode => {
          val len = r.getUByte()
          val items = (0 until len).map(_ => deserialize(r))
          STuple(items)
        }
        case SAny.typeCode => SAny
        case SUnit.typeCode => SUnit
        case SBox.typeCode => SBox
        case SAvlTree.typeCode => SAvlTree
        case _ =>
          sys.error(s"Cannot deserialize type starting from code $c")
      }
    }
    tpe
  }

  private def getArgType(r: ByteReader, primId: Int) =
    if (primId == 0)
      deserialize(r)
    else
      getEmbeddableType(primId)

  private def serializeTuple(t: STuple, w: ByteWriter) = {
    assert(t.items.length <= 255)
    w.put(STuple.TupleTypeCode)
    w.put(t.items.length.toByte)
    for (i <- t.items)
      serialize(i, w)
  }
}

