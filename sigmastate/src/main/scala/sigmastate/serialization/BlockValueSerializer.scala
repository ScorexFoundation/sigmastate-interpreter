package sigmastate.serialization

import sigmastate.Values._
import sigmastate._
import scorex.util.Extensions._
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import ValueSerializer._
import sigmastate.utils.SigmaByteWriter.{Vlq, U, DataInfo, ValDefFmt}

case class BlockValueSerializer(cons: (IndexedSeq[BlockItem], Value[SType]) => Value[SType])
  extends ValueSerializer[BlockValue] {
  override def opDesc = BlockValue
  val numItemsInfo: DataInfo[Vlq[U[Int]]] = ArgInfo("numItems", "number of block items")
  val itemInfo: DataInfo[ValDef] = ArgInfo("item_i", "block's definition in i-th position")
  val resultInfo: DataInfo[SValue] = ArgInfo("result", "result expression of the block")

  override def serialize(obj: BlockValue, w: SigmaByteWriter): Unit = {
    w.putUInt(obj.items.length, numItemsInfo)
    foreach(numItemsInfo.info.name, obj.items){ i =>
      ValueSerializer.addArgInfo(itemInfo)
      ValueSerializer.serialize(i, w)
    }
    w.putValue(obj.result, resultInfo)
  }

  override def parse(r: SigmaByteReader): Value[SType] = {
    val itemsSize = r.getUInt().toIntExact
    val values = (1 to itemsSize).map(_ => r.getValue().asInstanceOf[BlockItem])
    val result = r.getValue()
    cons(values, result)
  }
}
