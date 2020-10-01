package sigmastate.serialization

import sigmastate.Values._
import sigmastate._
import scorex.util.Extensions._
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import ValueSerializer._
import sigmastate.utils.SigmaByteWriter.{Vlq, U, DataInfo}
import spire.syntax.all.cfor

case class BlockValueSerializer(cons: (IndexedSeq[BlockItem], Value[SType]) => Value[SType])
  extends ValueSerializer[BlockValue] {
  override def opDesc = BlockValue
  val numItemsInfo: DataInfo[Vlq[U[Int]]] = ArgInfo("numItems", "number of block items")
  val itemInfo: DataInfo[SValue] = ArgInfo("item_i", "block's item in i-th position")
  val resultInfo: DataInfo[SValue] = ArgInfo("result", "result expression of the block")

  override def serialize(obj: BlockValue, w: SigmaByteWriter): Unit = {
    w.putUInt(obj.items.length, numItemsInfo)
    foreach(numItemsInfo.info.name, obj.items){ i =>
      w.putValue(i, itemInfo)
    }
    w.putValue(obj.result, resultInfo)
  }

  override def parse(r: SigmaByteReader): Value[SType] = {
    val itemsSize = r.getUInt().toIntExact
    val values = if (itemsSize == 0)
      BlockItem.EmptyArray
    else {
      // @hotspot: allocate new array only if it is not empty
      val buf = new Array[BlockItem](itemsSize)
      cfor(0)(_ < itemsSize, _ + 1) { i =>
        buf(i) = r.getValue().asInstanceOf[BlockItem]
      }
      buf
    }
    val result = r.getValue()
    cons(values, result)
  }
}
