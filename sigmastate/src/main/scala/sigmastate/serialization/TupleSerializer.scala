package sigmastate.serialization

import sigmastate.{ArgInfo, SType}
import sigmastate.Values._
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import ValueSerializer._
import sigmastate.utils.SigmaByteWriter.{DataInfo, U}
import spire.syntax.all.cfor

case class TupleSerializer(cons: Seq[Value[SType]] => Value[SType])
  extends ValueSerializer[Tuple] {
  override def opDesc = Tuple
  val numItemsInfo: DataInfo[U[Byte]] = ArgInfo("numItems", "number of items in the tuple")
  val itemInfo: DataInfo[SValue] = ArgInfo("item_i", "tuple's item in i-th position")

  override def serialize(obj: Tuple, w: SigmaByteWriter): Unit = {
    val length = obj.length
    w.putUByte(length, numItemsInfo)
    foreach(numItemsInfo.info.name, obj.items) { i =>
      w.putValue(i, itemInfo)
    }
  }

  override def parse(r: SigmaByteReader): Value[SType] = {
    val size = r.getByte()
    val values = new Array[SValue](size)
    cfor(0)(_ < size, _ + 1) { i =>
      values(i) = r.getValue()
    }
    cons(values)
  }

}
