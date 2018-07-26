package sigmastate.serialization

import sigmastate.{SCollection, SType}
import sigmastate.Values._
import sigmastate.serialization.OpCodes._
import sigmastate.utils.{ByteReader, ByteWriter}
import sigmastate.utils.Extensions._

case class ConcreteCollectionSerializer(cons: (IndexedSeq[Value[SType]], SType) => Value[SCollection[SType]])
  extends ValueSerializer[ConcreteCollection[_ <: SType]] {

  override val opCode: Byte = ConcreteCollectionCode

  override def serializeBody(cc: ConcreteCollection[_ <: SType], w: ByteWriter): Unit = {
    w.putUShort(cc.items.size)
    w.putType(cc.tpe.elemType)
    cc.items.foreach(w.putValue)
  }

  override def parseBody(r: ByteReader): Value[SCollection[SType]] = {
    val size = r.getUShort()
    val tItem = r.getType()
    val values =  (1 to size).map(_ => r.getValue())
    assert(values.forall(_.tpe == tItem), s"Invalid type of collection value")
    cons(values, tItem)
  }
}
