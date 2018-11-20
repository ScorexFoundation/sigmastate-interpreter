package sigmastate.serialization

import sigmastate.{SCollection, SType}
import sigmastate.Values._
import sigmastate.serialization.OpCodes._
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import scorex.util.Extensions._

case class ConcreteCollectionSerializer(cons: (IndexedSeq[Value[SType]], SType) => Value[SCollection[SType]])
  extends ValueSerializer[ConcreteCollection[_ <: SType]] {

  override val opCode: Byte = ConcreteCollectionCode

  override def serialize(cc: ConcreteCollection[_ <: SType], w: SigmaByteWriter): Unit = {
    w.putUShort(cc.items.size)
    w.putType(cc.tpe.elemType)
    cc.items.foreach(w.putValue)
  }

  override def parse(r: SigmaByteReader): Value[SCollection[SType]] = {
    val size = r.getUShort()
    val tItem = r.getType()
    val values =  (1 to size).map(_ => r.getValue())
    assert(values.forall(_.tpe == tItem), s"Invalid type of collection value in $values")
    cons(values, tItem)
  }
}
