package sigmastate.serialization

import sigmastate.SBoolean
import sigmastate.Values._
import sigmastate.serialization.OpCodes._
import sigmastate.utils.{ByteReader, ByteWriter}

object ConcreteCollectionBooleanConstantSerializer
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

  override def parseBody(r: ByteReader): ConcreteCollection[SBoolean.type] = {
    val size = r.getUShort()
    val booleanConstants = r.getBits(size).map(v => BooleanConstant.fromBoolean(v))
    ConcreteCollection(booleanConstants, SBoolean)
  }
}
