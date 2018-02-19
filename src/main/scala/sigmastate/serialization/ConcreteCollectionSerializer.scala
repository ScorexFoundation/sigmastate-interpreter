package sigmastate.serialization

import com.google.common.primitives.Chars
import sigmastate.{ConcreteCollection, SCollection, SType, Value}

object ConcreteCollectionSerializer extends SigmaSerializer[ConcreteCollection[_ <: SCollection[_ <: SType]]] {

  override val opCode: Byte = SigmaSerializer.ConcreteCollectionCode

  override def parseBody = {
    case (bytes, pos) =>
      val sizeBytes = bytes.slice(pos, pos + 2)
      val size = Chars.fromBytes(sizeBytes.head, sizeBytes.tail.head)
      val (values, consumed, types) = (1 to size).foldLeft((Seq[Value[SType]](), 2, Seq[SType.TypeCode]())) {
        case ((vs, c, ts), _) =>

          val (v, consumed, typeCode) = SigmaSerializer.deserialize(bytes, pos + c)
          (vs :+ v, c + consumed, ts :+ typeCode)
      }
      assert(Constraints.sameTypeN(types))
      (ConcreteCollection(values.toIndexedSeq), consumed, SCollection.collectionOf(types.head))
  }

  override def serializeBody = { cc =>
    require(cc.value.size <= Char.MaxValue)
    val size = cc.value.size.toChar // max collection size is Char.MaxValue = 65535

    val sizeBytes = Chars.toByteArray(size)

    cc.value.foldLeft(sizeBytes) { (bs, elem) =>
      bs ++ SigmaSerializer.serialize(elem)
    }
  }
}
