package sigmastate.serialization

import sigmastate.SType
import sigmastate.Values._
import sigmastate.serialization.Serializer.Position
import sigmastate.serialization.OpCodes._

object ConcreteCollectionSerializer extends ValueSerializer[ConcreteCollection[_ <: SType]] {

  override val opCode: Byte = ConcreteCollectionCode

  override def parseBody(bytes: Array[Byte], pos: Position): (ConcreteCollection[SType], Position) = {
    val r = Serializer.startReader(bytes, pos)
    val size = r.getShort()
    val tItem = r.getType()
    val values =  (1 to size).map(_ => r.getValue())
    assert(values.forall(_.tpe == tItem), s"Invalid type of collection value")
    (ConcreteCollection[SType](values)(tItem), r.consumed)
  }

  override def serializeBody(cc: ConcreteCollection[_ <: SType]): Array[Byte] = {
    val ccSize = cc.items.size
    require(ccSize <= Short.MaxValue, s"max collection size is Short.MaxValue = ${Short.MaxValue}")
    val size = ccSize.toShort
    val w = Serializer.startWriter()
        .putShort(size)
        .putType(cc.tpe.elemType)
    for (item <- cc.items) {
      w.putValue(item)
    }
    w.toBytes
  }
}
