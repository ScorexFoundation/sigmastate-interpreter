package sigmastate.serialization

import sigmastate.{SCollection, SType, ArgInfo}
import sigmastate.Values._
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import ValueSerializer._
import spire.syntax.all.cfor

case class ConcreteCollectionSerializer(cons: (IndexedSeq[Value[SType]], SType) => Value[SCollection[SType]])
  extends ValueSerializer[ConcreteCollection[_ <: SType]] {
  override def opDesc = ConcreteCollection

  override def serialize(cc: ConcreteCollection[_ <: SType], w: SigmaByteWriter): Unit = {
    w.putUShort(cc.items.size, ArgInfo("numItems", "number of item in a collection of expressions"))
    w.putType(cc.tpe.elemType, ArgInfo("elementType", "type of each expression in the collection"))
    foreach("numItems", cc.items)(w.putValue(_, ArgInfo("item_i", "expression in i-th position")))
  }

  override def parse(r: SigmaByteReader): Value[SCollection[SType]] = {
    val size = r.getUShort()   // READ
    val tItem = r.getType()    // READ
    val values = new Array[SValue](size)
    cfor(0)(_ < size, _ + 1) { i =>
      values(i) = r.getValue() // READ
    }
    assert(values.forall(_.tpe == tItem), s"Invalid type of collection value in $values")
    cons(values, tItem)
  }
}
