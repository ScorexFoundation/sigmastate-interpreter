package sigmastate.serialization

import sigmastate.Values._
import sigmastate._
import scorex.util.Extensions._
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import ValueSerializer._

case class BlockValueSerializer(cons: (IndexedSeq[BlockItem], Value[SType]) => Value[SType])
  extends ValueSerializer[BlockValue] {
  override def opDesc = BlockValue

  override def serialize(obj: BlockValue, w: SigmaByteWriter): Unit = {
    val sizeVar = "numItems"
    w.putUInt(obj.items.length, ArgInfo(sizeVar, "number of block items"))
    foreach(sizeVar, obj.items){ i =>
      w.putValue(i, ArgInfo("item_i", "block's item in i-th position"))
    }
    w.putValue(obj.result, ArgInfo("result", "result expression of the block"))
  }

  override def parse(r: SigmaByteReader): Value[SType] = {
    val itemsSize = r.getUInt().toIntExact
    val values = (1 to itemsSize).map(_ => r.getValue().asInstanceOf[BlockItem])
    val result = r.getValue()
    cons(values, result)
  }
}
