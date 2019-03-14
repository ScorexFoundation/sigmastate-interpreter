package sigmastate.serialization

import sigmastate.{SBoolean, SCollection}
import sigmastate.Values._
import sigmastate.serialization.OpCodes._
import sigmastate.utils.{SerializeLog, SigmaByteReader, SigmaByteWriter}

case class ConcreteCollectionBooleanConstantSerializer(cons: (IndexedSeq[Value[SBoolean.type]], SBoolean.type) => Value[SCollection[SBoolean.type]])
  extends ValueSerializer[ConcreteCollection[SBoolean.type]] {

  override val opCode: Byte = ConcreteCollectionBooleanConstantCode

  override def serializeBody(cc: ConcreteCollection[SBoolean.type], w: SigmaByteWriter): Unit = {
    SerializeLog.logPrintf(true, true, false, "ConcreteCollectionBooleanConstant")

    SerializeLog.logPrintf(true, true, false, "items.size")
    w.putUShort(cc.items.size)
    SerializeLog.logPrintf(false, true, false, "items.size")

    SerializeLog.logPrintf(true, true, false, "items")
    w.putBits(
      cc.items.map {
        case v: BooleanConstant => v.value
        case v => error(s"Expected collection of BooleanConstant values, got: $v")
      }.toArray)
    SerializeLog.logPrintf(false, true, false, "items")

    SerializeLog.logPrintf(false, true, false, "ConcreteCollectionBooleanConstant")
  }

  override def parseBody(r: SigmaByteReader): Value[SCollection[SBoolean.type]] = {
    val size = r.getUShort()
    val booleanConstants = r.getBits(size).map(v => BooleanConstant.fromBoolean(v))
    cons(booleanConstants, SBoolean)
  }
}
