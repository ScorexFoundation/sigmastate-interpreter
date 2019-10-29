package sigmastate.serialization

import sigmastate.{SCollection, ArgInfo, SType}
import sigmastate.Values._
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import ValueSerializer._
import sigmastate.utils.SigmaByteWriter.{U, Vlq, DataInfo}
import spire.syntax.all.cfor

case class ConcreteCollectionSerializer(cons: (IndexedSeq[Value[SType]], SType) => Value[SCollection[SType]])
  extends ValueSerializer[ConcreteCollection[_ <: SType]] {
  override val opDesc = ConcreteCollection

  val numItemsInfo: DataInfo[Vlq[U[Short]]] = ArgInfo("numItems", "number of item in a collection of expressions")
  val elementTypeInfo: DataInfo[SType] = ArgInfo("elementType", "type of each expression in the collection")
  val itemInfo: DataInfo[SValue] = ArgInfo("item_i", "expression in i-th position")

  override def serialize(cc: ConcreteCollection[_ <: SType], w: SigmaByteWriter): Unit = {
    w.putUShort(cc.items.size, numItemsInfo)
    w.putType(cc.tpe.elemType, elementTypeInfo)
    foreach(numItemsInfo.info.name, cc.items)(w.putValue(_, itemInfo))
  }

  /** @hotspot don't beautify this code */
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
