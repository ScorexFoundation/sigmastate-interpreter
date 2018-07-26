package sigmastate.serialization

import sigmastate.{SBoolean, SCollection}
import sigmastate.Values._
import sigmastate.serialization.OpCodes._
import sigmastate.utils.{ByteReader, ByteWriter}

case class ConcreteCollectionBooleanConstantSerializer(cons: (IndexedSeq[Value[SBoolean.type]], SBoolean.type) => Value[SCollection[SBoolean.type]])
  extends ValueSerializer[ConcreteCollection[SBoolean.type]] {

  override val opCode: Byte = ConcreteCollectionBooleanConstantCode

  override def serializeBody(cc: ConcreteCollection[SBoolean.type], w: ByteWriter): Unit = {
    w.putUShort(cc.items.size)
    w.putBits(
      cc.items.map {
        case v: BooleanConstant => v.value
        case v => error(s"Expected collection of BooleanConstant values, got: $v")
      }.toArray)
  }

  override def parseBody(r: ByteReader): Value[SCollection[SBoolean.type]] = {
    val size = r.getUShort()
    val booleanConstants = r.getBits(size).map(v => BooleanConstant.fromBoolean(v))
    cons(booleanConstants, SBoolean)
  }
}
