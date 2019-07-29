package sigmastate.serialization

import sigmastate.{SCollection, SBoolean, ArgInfo}
import sigmastate.Values._
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import SigmaByteWriter._
import spire.syntax.all.cfor

case class ConcreteCollectionBooleanConstantSerializer(cons: (IndexedSeq[Value[SBoolean.type]], SBoolean.type) => Value[SCollection[SBoolean.type]])
  extends ValueSerializer[ConcreteCollection[SBoolean.type]] {
  override def opDesc = ConcreteCollectionBooleanConstant

  override def serialize(cc: ConcreteCollection[SBoolean.type], w: SigmaByteWriter): Unit = {
    w.putUShort(cc.items.size, ArgInfo("numBits", "number of items in a collection of Boolean values"))
    w.putBits(
      cc.items.map {
        case v: BooleanConstant => v.value
        case v => error(s"Expected collection of BooleanConstant values, got: $v")
      }.toArray,
      maxBitsInfo("bits", 0x1FFF, "Boolean values encoded as as bits (right most byte is zero-padded on the right)"))
  }

  /** @hotspot don't beautify this code */
  override def parse(r: SigmaByteReader): Value[SCollection[SBoolean.type]] = {
    val size = r.getUShort()    // READ
    val bits = r.getBits(size)  // READ
    val items = new Array[BoolValue](size)
    cfor(0)(_ < size, _ + 1) { i =>
      items(i) = BooleanConstant.fromBoolean(bits(i))
    }
    cons(items, SBoolean)
  }
}
