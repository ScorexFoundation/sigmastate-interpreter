package sigmastate.serialization

import sigmastate.SType
import sigmastate.Values._
import sigmastate.serialization.OpCodes._
import sigmastate.utils.{ByteReader, ByteWriter}

object ConcreteCollectionSerializer extends ValueSerializer[ConcreteCollection[_ <: SType]] {

  override val opCode: Byte = ConcreteCollectionCode

  override def parseBody(r: ByteReader): ConcreteCollection[SType] = {
    val size = r.getUShort()
    val tItem = r.getType()
    val values =  (1 to size).map(_ => r.getValue())
    assert(values.forall(_.tpe == tItem), s"Invalid type of collection value")
    ConcreteCollection[SType](values)(tItem)
  }

  override def serializeBody(cc: ConcreteCollection[_ <: SType], w: ByteWriter): Unit = {
    val ccSize = cc.items.size
    require(ccSize <= Short.MaxValue, s"max collection size is Short.MaxValue = ${Short.MaxValue}")
    w.putUShort(ccSize.toShort)
    w.putType(cc.tpe.elemType)
    cc.items.foreach(w.putValue)
  }
}
