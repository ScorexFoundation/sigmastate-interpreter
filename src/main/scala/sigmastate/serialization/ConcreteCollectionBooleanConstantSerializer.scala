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
    val values = ConcreteCollection(r.getBits(size).map(v => Constant[SBoolean.type](v, SBoolean)), SBoolean)
    (values, r.consumed)
  }

  override def serializeBody(cc: ConcreteCollection[SBoolean.type]): Array[Byte] = {
    val ccSize = cc.items.size
    require(ccSize <= Short.MaxValue, s"max collection size is Short.MaxValue = ${Short.MaxValue}")
    val size = ccSize.toShort
    val w = Serializer.startWriter()
      .putUShort(size)
      // todo what's the difference between BooleanConstant and Constant[SBoolean.type]? Both might be in ConcreteCollection.
      .putBits(
      cc.items.map {
        case v: Constant[SBoolean.type] => v.value
        case v: BooleanConstant => v.value
      }.toArray)
    w.toBytes
  }
}
