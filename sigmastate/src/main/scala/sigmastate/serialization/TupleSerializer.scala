package sigmastate.serialization

import sigmastate.{ArgInfo, SType}
import sigmastate.Values._
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import ValueSerializer._
import sigmastate.utils.SigmaByteWriter.{DataInfo, U}

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
    val values =  (1 to size).map(_ => r.getValue())
    cons(values)
  }

}
