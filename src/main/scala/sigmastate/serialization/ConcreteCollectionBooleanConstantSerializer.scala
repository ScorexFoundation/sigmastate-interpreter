package sigmastate.serialization

import sigmastate.{SCollection, SBoolean}
import sigmastate.Values._
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}

case class ConcreteCollectionBooleanConstantSerializer(cons: (IndexedSeq[Value[SBoolean.type]], SBoolean.type) => Value[SCollection[SBoolean.type]])
  extends ValueSerializer[ConcreteCollection[SBoolean.type]] {
  override def opDesc = ConcreteCollectionBooleanConstant

  override def serialize(cc: ConcreteCollection[SBoolean.type], w: SigmaByteWriter): Unit = {
    w.putUShort(cc.items.size)
    w.putBits(
      cc.items.map {
        case v: BooleanConstant => v.value
        case v => error(s"Expected collection of BooleanConstant values, got: $v")
      }.toArray)
  }

  override def parse(r: SigmaByteReader): Value[SCollection[SBoolean.type]] = {
    val size = r.getUShort()
    val booleanConstants = r.getBits(size).map(v => BooleanConstant.fromBoolean(v))
    cons(booleanConstants, SBoolean)
  }
}
