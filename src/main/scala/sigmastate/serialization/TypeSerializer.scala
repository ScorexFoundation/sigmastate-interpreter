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

  override def serialize(tpe: SType, w: ByteWriter) = tpe match {
    case p: SEmbeddable => w.put(p.typeCode)
    case SAny => w.put(SAny.typeCode)
    case SUnit => w.put(SUnit.typeCode)
    case SBox => w.put(SBox.typeCode)
    case SAvlTree => w.put(SAvlTree.typeCode)
    case c: SCollection[a] => c.elemType match {
      case p: SEmbeddable =>
        val code = (SCollection.CollectionTypeCode + p.typeCode).toByte
        w.put(code)
      case cn: SCollection[a] => cn.elemType match {
        case p: SEmbeddable =>
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
      case p: SEmbeddable =>
        val code = (SOption.OptionTypeCode + p.typeCode).toByte
        w.put(code)
      case c: SCollection[a] => c.elemType match {
        case p: SEmbeddable =>
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
      case (p: SEmbeddable, _) =>
        if (p == t2) {
          // Symmetric pair of primitive types (`(Int, Int)`, `(Byte,Byte)`, etc.)
          val code = (STuple.PairSymmetricTypeCode + p.typeCode).toByte
          w.put(code)
        } else {
          // Pair of types where first is primitive (`(_, Int)`)
          val code = (STuple.Pair1TypeCode + p.typeCode).toByte
          w.put(code)
          serialize(t2, w)
        }
      case (_, p: SEmbeddable) =>
        // Pair of types where second is primitive (`(Int, _)`)
        val code = (STuple.Pair2TypeCode + p.typeCode).toByte
        w.put(code)
        serialize(t1, w)
      case _ =>
        // Pair of non-primitive types (`((Int, Byte), (Boolean,Box))`, etc.)
        w.put(STuple.Pair1TypeCode)
        serialize(t1, w)
        serialize(t2, w)
    }
    case STuple(items) if items.length < 2 =>
      sys.error(s"Invalid Tuple type with less than 2 items $items")
    case tup: STuple => tup.items.length match {
      case 3 =>
        // Triple of types
        w.put(STuple.TripleTypeCode)
        for (i <- tup.items)
          serialize(i, w)
      case 4 =>
        // Quadruple of types
        w.put(STuple.QuadrupleTypeCode)
        for (i <- tup.items)
          serialize(i, w)
      case _ =>
        // `Tuple` type with more than 4 items `(Int, Byte, Box, Boolean, Int)`
        serializeTuple(tup, w)
    }
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
        case STuple.Pair1TypeConstrId => // (_, t2)
          val (t1, t2) = if (primId == 0) {
            // Pair of non-primitive types (`((Int, Byte), (Boolean,Box))`, etc.)
            (deserialize(r), deserialize(r))
          } else {
            // Pair of types where first is primitive (`(_, Int)`)
            (getEmbeddableType(primId), deserialize(r))
          }
          STuple(t1, t2)
        case STuple.Pair2TypeConstrId => // (t1, _)
          if (primId == 0) {
            // Triple of types
            val (t1, t2, t3) = (deserialize(r), deserialize(r), deserialize(r))
            STuple(t1, t2, t3)
          } else {
            // Pair of types where second is primitive (`(Int, _)`)
            val t2 = getEmbeddableType(primId)
            val t1 = deserialize(r)
            STuple(t1, t2)
          }
        case STuple.PairSymmetricTypeConstrId => // (_, _)
          if (primId == 0) {
            // Quadruple of types
            val (t1, t2, t3, t4) = (deserialize(r), deserialize(r), deserialize(r), deserialize(r))
            STuple(t1, t2, t3, t4)
          } else {
            // Symmetric pair of primitive types (`(Int, Int)`, `(Byte,Byte)`, etc.)
            val t = getEmbeddableType(primId)
            STuple(t, t)
          }
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

