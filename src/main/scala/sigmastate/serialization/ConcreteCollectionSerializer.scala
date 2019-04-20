package sigmastate.serialization

import sigmastate.{SCollection, SType, ArgInfo}
import sigmastate.Values._
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import ValueSerializer._

case class ConcreteCollectionSerializer(cons: (IndexedSeq[Value[SType]], SType) => Value[SCollection[SType]])
  extends ValueSerializer[ConcreteCollection[_ <: SType]] {
  override def opDesc = ConcreteCollection

  override def serialize(cc: ConcreteCollection[_ <: SType], w: SigmaByteWriter): Unit = {
    w.putUShort(cc.items.size, ArgInfo("numItems", "number of item in a collection of expressions"))
    w.putType(cc.tpe.elemType, ArgInfo("elementType", "type of each expression in the collection"))
    foreach("numItems", cc.items)(w.putValue(_, ArgInfo("item_i", "expression in i-th position")))
  }

  override def parse(r: SigmaByteReader): Value[SCollection[SType]] = {
    val size = r.getUShort()
    val tItem = r.getType()
    val values =  (1 to size).map(_ => r.getValue())
    assert(values.forall(_.tpe == tItem), s"Invalid type of collection value in $values")
    cons(values, tItem)
  }
}
