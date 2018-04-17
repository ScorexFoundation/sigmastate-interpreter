package sigmastate.serialization

import com.google.common.primitives.Chars
import sigmastate.SType
import sigmastate.Values._
import sigmastate.serialization.ValueSerializer.Position

import scala.collection.mutable.ArrayBuffer

object ConcreteCollectionSerializer extends ValueSerializer[ConcreteCollection[_ <: SType]] {

  override val opCode: Byte = ValueSerializer.ConcreteCollectionCode

  override def parseBody(bytes: Array[Byte], pos: Position): (ConcreteCollection[SType], Position) = {
    val size = Chars.fromBytes(bytes(pos), bytes(pos + 1))
    val (tItem, tItemLen) = STypeSerializer.deserialize(bytes, pos + 2)
    val (values, typeCodes, consumed) = (1 to size).foldLeft((Seq[Value[SType]](), Seq[SType.TypeCode](), 2 + tItemLen)) {
      case ((vs, ts, c), _) =>
        val (v, consumed) = ValueSerializer.deserialize(bytes, pos + c)
        (vs :+ v, ts :+ v.tpe.typeCode, c + consumed)
    }
    assert(Constraints.sameTypeN(typeCodes))
    (ConcreteCollection[SType](values.toIndexedSeq)(tItem), consumed)
  }

  override def serializeBody(cc: ConcreteCollection[_ <: SType]): Array[Byte] = {
    val ccSize = cc.items.size
    require(ccSize <= Char.MaxValue, "max collection size is Char.MaxValue = 65535")
    val elemTpeBytes = STypeSerializer.serialize(cc.tpe.elemType)
    val size = ccSize.toChar
    val sizeBytes = Chars.toByteArray(size)
    val b = ArrayBuffer[Byte]()
    b ++= sizeBytes
    b ++= elemTpeBytes
    for (item <- cc.items) {
      b ++= ValueSerializer.serialize(item)
    }
    b.toArray
  }
}
