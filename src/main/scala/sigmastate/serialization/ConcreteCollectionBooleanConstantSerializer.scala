package sigmastate.serialization

import sigmastate.SBoolean
import sigmastate.Values._
import sigmastate.serialization.OpCodes._
import sigmastate.serialization.Serializer.Position

object ConcreteCollectionBooleanConstantSerializer
  extends ValueSerializer[ConcreteCollection[SBoolean.type]] {

  override val opCode: Byte = ConcreteCollectionBooleanConstantCode

  override def parseBody(bytes: Array[Byte], pos: Position): (ConcreteCollection[SBoolean.type], Position) = {
    val r = Serializer.startReader(bytes, pos)
    val size = r.getUShort()
    val booleanConstants = r.getBits(size).map(v => BooleanConstant.fromBoolean(v))
    (ConcreteCollection(booleanConstants, SBoolean), r.consumed)
  }

  override def serializeBody(cc: ConcreteCollection[SBoolean.type]): Array[Byte] = {
    val ccSize = cc.items.size
    require(ccSize <= Short.MaxValue, s"max collection size is Short.MaxValue = ${Short.MaxValue}")
    val size = ccSize.toShort
    val w = Serializer.startWriter()
      .putUShort(size)
      .putBits(
      cc.items.map {
        case v: BooleanConstant => v.value
        case _ =>
          ??? // fail with error (this should never happen, see guard in ConcreteCollection.opcode
      }.toArray)
    w.toBytes
  }
}
