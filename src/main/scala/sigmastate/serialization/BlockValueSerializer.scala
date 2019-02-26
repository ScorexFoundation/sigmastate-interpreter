package sigmastate.serialization

import sigmastate.Values._
import sigmastate._
import sigmastate.serialization.OpCodes._
import scorex.util.Extensions._
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}

case class BlockValueSerializer(cons: (IndexedSeq[BlockItem], Value[SType]) => Value[SType])
  extends ValueSerializer[BlockValue] {

  override val opCode: OpCode = BlockValueCode

  override def serialize(obj: BlockValue, w: SigmaByteWriter): Unit = {
    w.putUInt(obj.items.length)
    obj.items.foreach(w.putValue(_))
    w.putValue(obj.result)
  }

  override def parse(r: SigmaByteReader): Value[SType] = {
    val itemsSize = r.getUInt().toIntExact
    val values = (1 to itemsSize).map(_ => r.getValue().asInstanceOf[BlockItem])
    val result = r.getValue()
    cons(values, result)
  }
}
